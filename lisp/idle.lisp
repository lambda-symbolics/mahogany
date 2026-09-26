;;;; Screen blanking. The kernel already counts milliseconds since the last
;;;; wskbd/wsmouse event (machdep.lpsched.idle_ms on LISPBSD); a slow timer
;;;; reads it and turns the panels off after *idle-blank-seconds*, unless a
;;;; client holds an idle inhibitor (video). While blanked the timer runs
;;;; faster so the first keypress or nudge brings the panel back.
(in-package #:mahogany)

(defvar *idle-blank-seconds* 300 "Seconds of no input before the panel goes off. NIL disables.")
(defvar *idle-poll-ms* 10000 "Timer period while the panel is on.")
(defvar *idle-poll-blanked-ms* 500 "Timer period while the panel is off.")
(defvar *idle-sysctl* "machdep.lpsched.idle_ms")

(defvar *idle-timer* nil)
(defvar *idle-blanked* nil)
(defvar *idle-last-ms* 0)

(defun idle-ms ()
  "Milliseconds since the last input event, or NIL when unavailable."
  #+sbcl
  (handler-case
      (sb-alien:with-alien ((value sb-alien:unsigned-long)
                            (len sb-alien:unsigned-long))
        (setf value 0 len 8)
        (let ((rc (sb-alien:alien-funcall
                   (sb-alien:extern-alien
                    "sysctlbyname"
                    (function sb-alien:int sb-alien:c-string
                              (* sb-alien:unsigned-long) (* sb-alien:unsigned-long)
                              (* t) sb-alien:unsigned-long))
                   *idle-sysctl* (sb-alien:addr value) (sb-alien:addr len)
                   nil 0)))
          (when (zerop rc)
            (if (= len 4) (ldb (byte 32 0) value) value))))
    (error () nil))
  #-sbcl nil)

(defun idle-outputs ()
  (loop for container across (state-cur-outputs *compositor-state*)
        collect (tree:output-container-output container)))

(defun idle-blank ()
  (unless *idle-blanked*
    (log-string :info "Idle: panel off")
    (setf *idle-blanked* t)
    (dolist (output (idle-outputs))
      (hrt:output-set-power output nil))))

(defun idle-unblank ()
  (when *idle-blanked*
    (log-string :info "Idle: panel on")
    (setf *idle-blanked* nil)
    (dolist (output (idle-outputs))
      (hrt:output-set-power output t))))

(defun %idle-tick (timer)
  (let ((ms (idle-ms)))
    (when ms
      (cond
        (*idle-blanked*
         ;; Activity resets the counter, so it drops below the last reading.
         (when (< ms *idle-last-ms*)
           (idle-unblank)))
        ((and *idle-blank-seconds*
              (>= ms (* 1000 *idle-blank-seconds*))
              (not (hrt:idle-inhibited-p)))
         (idle-blank)))
      (setf *idle-last-ms* ms)))
  (hrt:timer-handle-update timer (if *idle-blanked* *idle-poll-blanked-ms* *idle-poll-ms*)))

(defun idle-start ()
  "Start the blanking timer. Safe to call again."
  (unless *idle-timer*
    (setf *idle-timer* (hrt:server-make-timer (state-server *compositor-state*) #'%idle-tick)))
  (hrt:timer-handle-update *idle-timer* *idle-poll-ms*))

(defcommand monitors-off ()
  (:documentation "Turn the panel off now; any input turns it back on")
  (:method ()
    (setf *idle-last-ms* (or (idle-ms) 0))
    (idle-blank)
    (when *idle-timer*
      (hrt:timer-handle-update *idle-timer* *idle-poll-blanked-ms*))))

(defcommand monitors-on ()
  (:documentation "Turn the panel on")
  (:method ()
    (idle-unblank)))
