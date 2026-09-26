;;;; Dynamic refresh rate. Panels such as the X1 Nano's advertise the same
;;;; resolution at 60 Hz and 30 Hz. When nothing on screen changes for a few
;;;; seconds the output is switched to the slow mode, which lowers the display
;;;; engine's and the panel link's power; the first rendered frame or input
;;;; event brings the fast mode back. heart counts rendered frames, so a
;;;; video, a blinking cursor or a moving pointer all count as activity.
(in-package #:mahogany)

(defvar *refresh-enabled* nil "Switch outputs to the slow refresh when idle.")
(defvar *refresh-fast-mhz* 60000 "Refresh to use while the screen is changing.")
(defvar *refresh-slow-mhz* 30000 "Refresh to use while it is not.")
(defvar *refresh-idle-seconds* 3
  "Seconds without rendered frames before dropping to the slow refresh.")
(defvar *refresh-poll-ms* 1000)
(defvar *refresh-quiet-frames* 1
  "Frames per poll interval still regarded as quiet (a blinking cursor is
one or two a second).")

(defvar *refresh-timer* nil)
(defvar *refresh-state* (make-hash-table :test 'equal)
  "Output name -> (frames-at-last-poll quiet-polls slow-p).")
(defvar *refresh-activity* nil "Set by input handlers, consumed by the timer.")

(defun refresh-note-activity ()
  "Called from the input handlers on every key, button or wheel event."
  (setf *refresh-activity* t)
  ;; Do not wait for the next poll: the user is here.
  (when (and *refresh-enabled* *refresh-timer*)
    (hrt:timer-handle-update *refresh-timer* 1)))

(defun %refresh-set (output fast)
  (let* ((want (if fast *refresh-fast-mhz* *refresh-slow-mhz*))
         (got (hrt:output-set-refresh output want)))
    (log-string :debug "Refresh: ~A -> ~A mHz~:[ (failed)~;~]"
                (hrt:output-full-name output) want (plusp got))
    (plusp got)))

(defun %refresh-tick (timer)
  (let ((activity *refresh-activity*))
    (setf *refresh-activity* nil)
    (when (and *refresh-enabled* (not *idle-blanked*))
      (loop for container across (state-cur-outputs *compositor-state*)
            for output = (tree:output-container-output container)
            for name = (hrt:output-full-name output)
            for frames = (hrt:output-frames-rendered output)
            for entry = (or (gethash name *refresh-state*)
                            (setf (gethash name *refresh-state*) (list frames 0 nil)))
            do (destructuring-bind (last quiet slow) entry
                 (let ((delta (- frames last)))
                   (cond
                     (slow
                      (when (or activity (> delta *refresh-quiet-frames*))
                        (when (%refresh-set output t)
                          (setf (third entry) nil (second entry) 0))))
                     (t
                      (if (or activity (> delta *refresh-quiet-frames*))
                          (setf (second entry) 0)
                          (incf (second entry)))
                      (when (>= (* (second entry) *refresh-poll-ms*)
                                (* 1000 *refresh-idle-seconds*))
                        (when (%refresh-set output nil)
                          (setf (third entry) t)))))
                   (setf (first entry) frames))))))
  (hrt:timer-handle-update timer *refresh-poll-ms*))

(defun refresh-start ()
  "Start the refresh policy timer. Safe to call again."
  (setf *refresh-enabled* t)
  (unless *refresh-timer*
    (setf *refresh-timer*
          (hrt:server-make-timer (state-server *compositor-state*) #'%refresh-tick)))
  (hrt:timer-handle-update *refresh-timer* *refresh-poll-ms*))

(defun refresh-stop ()
  "Stop the policy and put every output back on the fast refresh."
  (setf *refresh-enabled* nil)
  (loop for container across (state-cur-outputs *compositor-state*)
        do (%refresh-set (tree:output-container-output container) t))
  (clrhash *refresh-state*))

(defcommand refresh-toggle ()
  (:documentation "Turn dynamic refresh on or off")
  (:method ()
    (if *refresh-enabled* (refresh-stop) (refresh-start))
    (toast-message *compositor-state*
                   (format nil "Dynamic refresh ~:[off~;on~]" *refresh-enabled*))))
