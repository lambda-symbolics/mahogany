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
(in-package #:mahogany)

(defvar *refresh-enabled* nil "Pick the slow mode for steady ~30 fps content.")
(defvar *refresh-fast-mhz* 60000)
(defvar *refresh-slow-mhz* 30000)
(defvar *refresh-poll-ms* 1000)
(defvar *refresh-cadence-min* 28 "Frames per second that count as 30 fps content.")
(defvar *refresh-cadence-max* 31)
(defvar *refresh-cadence-seconds* 4
  "Consecutive seconds of 30 fps content before switching to the slow mode.")
(defvar *refresh-input-hold-seconds* 2
  "Seconds after the last input during which the fast mode is kept.")
(defvar *refresh-probe-seconds* 10
  "At the slow mode, how often to re-measure the content at the fast mode.")
(defvar *refresh-probe-length* 2 "Seconds a probe stays at the fast mode.")

(defvar *refresh-timer* nil)
(defvar *refresh-state* (make-hash-table :test 'equal)
  "Output name -> refresh-output.")
(defvar *refresh-last-input* 0 "get-internal-real-time of the last input.")
(defvar *refresh-busy* nil "Set by a poll that saw frames or a slow mode.")
(defvar *refresh-idle-poll-ms* 5000 "Poll period while nothing renders.")

(defstruct refresh-output
  (frames 0)            ; frame counter at the last poll
  (cadence 0)           ; consecutive polls that looked like 30 fps content
  (slow nil)            ; the slow mode is selected
  (slow-polls 0)        ; polls spent at the slow mode since the last probe
  (probe 0))            ; polls left in the current probe at the fast mode

(defun %refresh-seconds-since-input ()
  (/ (- (get-internal-real-time) *refresh-last-input*)
     internal-time-units-per-second))

(defun refresh-note-activity ()
  "Called from the input handlers on every key, button or wheel event."
  (setf *refresh-last-input* (get-internal-real-time))
  (when (and *refresh-enabled* *refresh-timer*)
    (loop for entry being the hash-values of *refresh-state*
          when (refresh-output-slow entry)
            do (hrt:timer-handle-update *refresh-timer* 1)
               (return))))

(defun %refresh-set (output fast)
  (let* ((want (if fast *refresh-fast-mhz* *refresh-slow-mhz*))
         (got (hrt:output-set-refresh output want)))
    (log-string :debug "Refresh: ~A -> ~A mHz~:[ (failed)~;~]"
                (hrt:output-full-name output) want (plusp got))
    (plusp got)))

(defun %refresh-go-fast (output entry)
  (when (%refresh-set output t)
    (setf (refresh-output-slow entry) nil
          (refresh-output-cadence entry) 0
          (refresh-output-slow-polls entry) 0)))

(defun %refresh-poll-output (output entry)
  (let* ((frames (hrt:output-frames-rendered output))
         (rate (- frames (refresh-output-frames entry)))
         (typing (< (%refresh-seconds-since-input) *refresh-input-hold-seconds*))
         (cadence (<= *refresh-cadence-min* rate *refresh-cadence-max*)))
    (setf (refresh-output-frames entry) frames)
    (when (or (plusp rate) (refresh-output-slow entry) (plusp (refresh-output-probe entry)))
      (setf *refresh-busy* t))
    (cond
      ;; Slow mode: leave it on input, when the content stops looking like
      ;; 30 fps, or for a periodic probe at the fast mode.
      ((refresh-output-slow entry)
       (incf (refresh-output-slow-polls entry))
       (cond ((or typing (not cadence))
              (%refresh-go-fast output entry))
             ((>= (* (refresh-output-slow-polls entry) *refresh-poll-ms*)
                  (* 1000 *refresh-probe-seconds*))
              (when (%refresh-set output t)
                (setf (refresh-output-slow entry) nil
                      (refresh-output-slow-polls entry) 0
                      (refresh-output-probe entry)
                      (ceiling (* 1000 *refresh-probe-length*) *refresh-poll-ms*))))))
      ;; A probe measures the true rate before cadence may count again.
      ((plusp (refresh-output-probe entry))
       (decf (refresh-output-probe entry))
       (setf (refresh-output-cadence entry)
             (if (and cadence (not typing)) *refresh-cadence-seconds* 0)))
      ((and cadence (not typing))
       (incf (refresh-output-cadence entry)))
      (t
       (setf (refresh-output-cadence entry) 0)))
    (when (and (not (refresh-output-slow entry))
               (zerop (refresh-output-probe entry))
               (>= (refresh-output-cadence entry) *refresh-cadence-seconds*))
      (when (%refresh-set output nil)
        (setf (refresh-output-slow entry) t
              (refresh-output-slow-polls entry) 0)))))

(defun %refresh-tick (timer)
  (setf *refresh-busy* nil)
  (when (and *refresh-enabled* (not *idle-blanked*))
    (loop for container across (state-cur-outputs *compositor-state*)
          for output = (tree:output-container-output container)
          for name = (hrt:output-full-name output)
          for entry = (or (gethash name *refresh-state*)
                          (setf (gethash name *refresh-state*)
                                (make-refresh-output
                                 :frames (hrt:output-frames-rendered output))))
          do (%refresh-poll-output output entry)))
  ;; No wakeup a second on a still screen: DRRS has that case anyway.
  (hrt:timer-handle-update timer (if *refresh-busy* *refresh-poll-ms*
                                     *refresh-idle-poll-ms*)))

(defun refresh-start ()
  "Start the refresh policy timer. Safe to call again."
  (setf *refresh-enabled* t)
  (unless *refresh-timer*
    (setf *refresh-timer*
          (hrt:server-make-timer (state-server *compositor-state*) #'%refresh-tick)))
  (hrt:timer-handle-update *refresh-timer* *refresh-poll-ms*))

(defun refresh-stop ()
  "Stop the policy and put every output back on the fast mode."
  (setf *refresh-enabled* nil)
  (loop for container across (state-cur-outputs *compositor-state*)
        do (%refresh-set (tree:output-container-output container) t))
  (clrhash *refresh-state*))

(defcommand refresh-toggle ()
  (:documentation "Turn the 30 fps content matching on or off")
  (:method ()
    (if *refresh-enabled* (refresh-stop) (refresh-start))
    (toast-message *compositor-state*
                   (format nil "Refresh matching ~:[off~;on~]" *refresh-enabled*))))
