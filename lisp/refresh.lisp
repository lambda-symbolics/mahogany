;;;; Dynamic refresh rate, working together with the kernel's DRRS.
;;;;
;;;; The Nano's panel runs the same timings at 60 Hz or 30 Hz; only the pixel
;;;; clock differs. The LISPBSD i915 switches between them seamlessly, in two
;;;; ways:
;;;;
;;;;  - Idleness DRRS in the kernel: one second after the last screen update
;;;;    the pipe drops to 30 Hz, and the next update brings 60 Hz back before
;;;;    it is shown. This covers a still screen completely, so the compositor
;;;;    leaves it alone.
;;;;
;;;;  - Choosing the 30 Hz mode explicitly, which is what this file does, for
;;;;    the one case DRRS cannot see: content that updates steadily about 30
;;;;    times a second, such as 30 fps video. Its frames arrive every 33 ms,
;;;;    so DRRS never gets its idle second, yet 30 Hz shows every frame.
;;;;
;;;; At 30 Hz the measured rate is capped at 30, so faster content would hide.
;;;; Any key, click or wheel event returns to 60 Hz at once, and a short
;;;; probe at 60 Hz every *refresh-probe-seconds* re-measures the content.
;;;; 24 and 25 fps video stays at 60 Hz: at 30 Hz one frame in four or five
;;;; would stay up twice as long.
;;;;
;;;; Reading mode trades smoothness nobody is watching for power: on battery,
;;;; once there has been no input for *refresh-reading-seconds*, content that
;;;; animates faster than 30 fps (a page streaming text, a spinner) is shown
;;;; at 30 Hz too, so its client draws half the frames.  It stays there
;;;; without probing until the next input; a client holding an idle
;;;; inhibitor (video playback) is never slowed this way.
(in-package #:mahogany)

(defvar *refresh-enabled* nil "Pick the slow mode for steady ~30 fps content.")
(defvar *refresh-fast-mhz* 60000)
(defvar *refresh-slow-mhz* 30000)
(defvar *refresh-poll-ms* 1000)
(defvar *refresh-cadence-min* 28 "Frames per second that count as 30 fps content.")
(defvar *refresh-cadence-max* 31)
;; All periods below are measured in elapsed time, not in polls: the poll
;; period changes between *refresh-poll-ms* and *refresh-idle-poll-ms*, and
;; input moves the next poll forward.
(defvar *refresh-cadence-seconds* 4
  "Seconds of steady 30 fps content before switching to the slow mode.")
(defvar *refresh-input-hold-seconds* 2
  "Seconds after the last input during which the fast mode is kept.")
(defvar *refresh-probe-seconds* 10
  "At the slow mode, how often to re-measure the content at the fast mode.")
(defvar *refresh-probe-length* 2 "Seconds a probe stays at the fast mode.")
;; NIL turns reading mode off.
(defvar *refresh-reading-seconds* 15
  "On battery, seconds without input before animation is shown at 30 Hz.")
(defvar *refresh-ac-file* "/var/run/lpsched.ac"
  "Holds 1 while the charger is connected (written by the powerd hook).")
(defvar *refresh-min-sample-seconds* 1/2
  "A shorter interval since the last sample is not measured: a poll moved
forward by input would see a frame rate made of one or two frames.")

(defvar *refresh-timer* nil)
(defvar *refresh-state* (make-hash-table :test 'equal)
  "Output name -> refresh-output.")
(defvar *refresh-last-input* 0 "get-internal-real-time of the last input.")
(defvar *refresh-busy* nil "Set by a poll that saw animation or a slow mode.")
(defvar *refresh-idle-poll-ms* 5000 "Poll period while nothing animates.")
(defvar *refresh-busy-rate* 10
  "Frames per second that count as animation.  The bar and the odd redraw
stay below it, so a static desktop keeps the slow poll.")

(defstruct refresh-output
  (frames 0)            ; frame counter at the last sample
  (stamp 0)             ; get-internal-real-time of the last sample
  (cadence-since nil)   ; when the content started to look like 30 fps
  (slow nil)            ; the slow mode is selected
  (reading nil)         ; ... by reading mode, not by the content's cadence
  (slow-since 0)        ; when the slow mode was selected
  (probe-until nil))    ; end of the current probe at the fast mode

(defun %refresh-now () (get-internal-real-time))

(defun %refresh-seconds (from to)
  (/ (- to from) internal-time-units-per-second))

(defun %refresh-seconds-since-input ()
  (%refresh-seconds *refresh-last-input* (%refresh-now)))

(defun refresh-note-activity ()
  "Called from the input handlers on every key, button or wheel event."
  (setf *refresh-last-input* (%refresh-now))
  (when (and *refresh-enabled* *refresh-timer*)
    (loop for entry being the hash-values of *refresh-state*
          when (refresh-output-slow entry)
            do (hrt:timer-handle-update *refresh-timer* 1)
               (return))))

;;; What the bar shows.  Besides the mode selected here, the kernel drops the
;;; panel to 30 Hz by itself one second after the last update, so a screen
;;; drawing less than a frame a second is shown as 30 Hz too.  The bar's own
;;; repaint is one frame in a poll interval of seconds, so showing the change
;;; cannot undo it.
(defvar *refresh-selected-hz* 60 "The mode last selected by %refresh-set.")
(defvar *refresh-last-fps* 60 "Frame rate of the panel over the last sample.")

(defun %refresh-show ()
  (let ((hz (if (or (eql *refresh-selected-hz* 30) (< *refresh-last-fps* 1)) 30 60)))
    (unless (eql hz *bar-refresh-hz*)
      (setf *bar-refresh-hz* hz)
      (bar-schedule-refresh))))

(defun %refresh-set (output fast)
  (let* ((want (if fast *refresh-fast-mhz* *refresh-slow-mhz*))
         (got (hrt:output-set-refresh output want)))
    (log-string :debug "Refresh: ~A -> ~A mHz~:[ (failed)~;~]"
                (hrt:output-full-name output) want (plusp got))
    (when (plusp got)
      (setf *refresh-selected-hz* (round want 1000))
      (%refresh-show))
    (plusp got)))

(defun %refresh-go-fast (output entry)
  (when (%refresh-set output t)
    (setf (refresh-output-slow entry) nil
          (refresh-output-reading entry) nil
          (refresh-output-cadence-since entry) nil)))

(defun %refresh-on-battery-p ()
  "True when the AC file says the charger is out; no file counts as AC."
  (handler-case
      (with-open-file (in *refresh-ac-file* :if-does-not-exist nil)
        (and in (equal (read-line in nil "") "0")))
    (error () nil)))

(defun %refresh-reading-p (fps)
  (and *refresh-reading-seconds*
       (> fps *refresh-cadence-max*)
       (>= (%refresh-seconds-since-input) *refresh-reading-seconds*)
       (not (hrt:idle-inhibited-p))
       (%refresh-on-battery-p)))

(defun %refresh-rebase (output entry now)
  "Start measuring afresh from NOW."
  (setf (refresh-output-frames entry) (hrt:output-frames-rendered output)
        (refresh-output-stamp entry) now
        (refresh-output-cadence-since entry) nil))

(defun %refresh-poll-output (output entry now)
  (let ((typing (< (%refresh-seconds-since-input) *refresh-input-hold-seconds*))
        (frames (hrt:output-frames-rendered output))
        (elapsed (%refresh-seconds (refresh-output-stamp entry) now)))
    ;; Input leaves the slow mode at once, whatever was measured.
    (when (and typing (refresh-output-slow entry))
      (%refresh-go-fast output entry)
      (%refresh-rebase output entry now)
      (setf *refresh-busy* t)
      (return-from %refresh-poll-output))
    (cond ((< frames (refresh-output-frames entry))
           ;; The counter started over (the output was set up again).
           (%refresh-rebase output entry now)
           (return-from %refresh-poll-output))
          ((< elapsed *refresh-min-sample-seconds*)
           (return-from %refresh-poll-output)))
    (let* ((fps (/ (- frames (refresh-output-frames entry)) elapsed))
           (cadence (<= *refresh-cadence-min* fps *refresh-cadence-max*)))
      (setf (refresh-output-frames entry) frames
            (refresh-output-stamp entry) now
            *refresh-last-fps* fps)
      (%refresh-show)
      ;; Reading mode needs no fast polls: input leaves it through
      ;; refresh-note-activity, which moves the next poll forward.
      (when (and (not (refresh-output-reading entry))
                 (or (>= fps *refresh-busy-rate*) (refresh-output-slow entry)
                     (refresh-output-probe-until entry)))
        (setf *refresh-busy* t))
      (cond
        ;; Reading mode ends with input (above), a video taking an idle
        ;; inhibitor, or the charger.
        ((refresh-output-reading entry)
         (when (or (hrt:idle-inhibited-p) (not (%refresh-on-battery-p)))
           (%refresh-go-fast output entry)))
        ((and (not (refresh-output-probe-until entry))
              (%refresh-reading-p fps))
         (when (%refresh-set output nil)
           (setf (refresh-output-slow entry) t
                 (refresh-output-reading entry) t
                 (refresh-output-slow-since entry) now)
           ;; Key, button and wheel events reach refresh-note-activity
           ;; anyway; this brings pointer motion too, which would
           ;; otherwise move the cursor at 30 Hz.
           (hrt:hrt-arm-activity-callback)))
        ;; Slow mode: leave it when the content stops looking like 30 fps,
        ;; or for a periodic probe at the fast mode.
        ((refresh-output-slow entry)
         (cond ((not cadence)
                (%refresh-go-fast output entry))
               ((>= (%refresh-seconds (refresh-output-slow-since entry) now)
                    *refresh-probe-seconds*)
                (when (%refresh-set output t)
                  (setf (refresh-output-slow entry) nil
                        (refresh-output-probe-until entry)
                        (+ now (* *refresh-probe-length*
                                  internal-time-units-per-second)))))))
        ;; A probe measures the true rate before cadence may count again;
        ;; what the last sample of the probe saw decides.
        ((refresh-output-probe-until entry)
         (setf (refresh-output-cadence-since entry)
               (and cadence (not typing)
                    (- now (* *refresh-cadence-seconds*
                              internal-time-units-per-second))))
         (when (>= now (refresh-output-probe-until entry))
           (setf (refresh-output-probe-until entry) nil)))
        ((and cadence (not typing))
         ;; The content has looked like 30 fps since the start of this sample.
         (unless (refresh-output-cadence-since entry)
           (setf (refresh-output-cadence-since entry)
                 (- now (round (* elapsed internal-time-units-per-second))))))
        (t
         (setf (refresh-output-cadence-since entry) nil)))
      (when (and (not (refresh-output-slow entry))
                 (null (refresh-output-probe-until entry))
                 (refresh-output-cadence-since entry)
                 (>= (%refresh-seconds (refresh-output-cadence-since entry) now)
                     *refresh-cadence-seconds*))
        (when (%refresh-set output nil)
          (setf (refresh-output-slow entry) t
                (refresh-output-slow-since entry) now))))))

(defun %refresh-entries (function)
  "Call FUNCTION with each output and its refresh-output, made on first use."
  (let ((now (%refresh-now)))
    (loop for container across (state-cur-outputs *compositor-state*)
          for output = (tree:output-container-output container)
          for name = (hrt:output-full-name output)
          for entry = (or (gethash name *refresh-state*)
                          (setf (gethash name *refresh-state*)
                                (make-refresh-output
                                 :frames (hrt:output-frames-rendered output)
                                 :stamp now)))
          do (funcall function output entry now))))

(defun %refresh-tick (timer)
  (setf *refresh-busy* nil)
  ;; Stopped: no rearming, so a disabled policy costs no wakeups.
  (unless *refresh-enabled*
    (return-from %refresh-tick))
  (unless *idle-blanked*
    (%refresh-entries #'%refresh-poll-output))
  ;; No wakeup a second on a still screen: DRRS has that case anyway.  While
  ;; the panel is off nothing needs measuring; idle-unblank rearms the timer.
  (hrt:timer-handle-update timer (cond (*idle-blanked* 0)
                                       (*refresh-busy* *refresh-poll-ms*)
                                       (t *refresh-idle-poll-ms*))))

(defun refresh-wake ()
  "Resume polling, after the panel comes back on.  The time the panel was
off is not measured: every output starts a new sample."
  (when (and *refresh-enabled* *refresh-timer*)
    (%refresh-entries #'%refresh-rebase)
    (hrt:timer-handle-update *refresh-timer* *refresh-poll-ms*)))

(defun refresh-start ()
  "Start the refresh policy timer. Safe to call again."
  (setf *refresh-enabled* t)
  (unless *refresh-timer*
    (setf *refresh-timer*
          (hrt:server-make-timer (state-server *compositor-state*) #'%refresh-tick)))
  (%refresh-entries #'%refresh-rebase)
  (hrt:timer-handle-update *refresh-timer* *refresh-poll-ms*))

(defun refresh-stop ()
  "Stop the policy and put every output back on the fast mode."
  (setf *refresh-enabled* nil)
  (when *refresh-timer*
    (hrt:timer-handle-update *refresh-timer* 0))
  (loop for container across (state-cur-outputs *compositor-state*)
        do (%refresh-set (tree:output-container-output container) t))
  (clrhash *refresh-state*))

(defcommand refresh-toggle ()
  (:documentation "Turn the 30 fps content matching on or off")
  (:method ()
    (if *refresh-enabled* (refresh-stop) (refresh-start))
    (toast-message *compositor-state*
                   (format nil "Refresh matching ~:[off~;on~]" *refresh-enabled*))))
