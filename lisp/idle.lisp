;;;; Idle power management: dim the backlight, then turn the panel off.
;;;;
;;;; The kernel counts milliseconds since the last wskbd/wsmouse event
;;;; (machdep.lpsched.idle_ms on LISPBSD). Nothing polls it: a one-shot
;;;; timer fires at the next deadline (dim or blank), and while the panel is
;;;; dimmed or off, heart calls back on the very next input event, so the
;;;; panel comes back without delay and without a wakeup loop.
;;;;
;;;; Blanking disables the output, which stops the display pipe entirely; the
;;;; windows are marked suspended so their clients stop their timers too, and
;;;; the status sampler is paused because nobody can see the bar. Closing the
;;;; lid does the same through lid-closed (called by the powerd lid script).
;;;; A client holding an idle inhibitor (video playback) keeps the panel on.
(in-package #:mahogany)

(defvar *idle-dim-seconds* 120
  "Seconds of no input before the backlight dims. NIL disables dimming.")
(defvar *idle-dim-level* 15
  "Backlight level (1-100) while dimmed. A lower level is left alone.")
(defvar *idle-blank-seconds* 300
  "Seconds of no input before the panel goes off. NIL disables blanking.")
(defvar *idle-inhibited-recheck-ms* 30000
  "While an idle inhibitor holds the panel on, look again this often.")
(defvar *idle-sysctl* "machdep.lpsched.idle_ms")
(defvar *idle-brightness* "/usr/local/bin/brightness"
  "The backlight script. It remembers the level per supply; dim lowers the
panel without touching that, undim and restore put it back.")
(defvar *idle-statusbard-pattern* "^/bin/sh /usr/local/bin/statusbard$")

(defvar *idle-timer* nil)
(defvar *idle-dimmed* nil)
(defvar *idle-blanked* nil)
(defvar *idle-lid-closed* nil)
(defvar *idle-baseline-ms* 0
  "The idle counter at session start. Deadlines count from here until the
first input event resets the counter, so a session started after a long
idle stretch does not dim or blank at once.")

(defun %sh (command)
  "Run COMMAND with /bin/sh in the background."
  (uiop:launch-program (list "/bin/sh" "-c" command)
                       :output nil :error-output nil))

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

(defun %idle-elapsed-ms ()
  "Milliseconds of idleness that count towards the deadlines."
  (let ((ms (idle-ms)))
    (cond ((null ms) 0)
          ((< ms *idle-baseline-ms*) (setf *idle-baseline-ms* 0) ms)
          (t (- ms *idle-baseline-ms*)))))

(defun idle-outputs ()
  (loop for container across (state-cur-outputs *compositor-state*)
        collect (tree:output-container-output container)))

;;; --- dimming -------------------------------------------------------------

(defun %brightness (&rest args)
  (%sh (format nil "~A~{ ~A~} >/dev/null 2>&1" *idle-brightness* args)))

(defun idle-dim ()
  (unless (or *idle-dimmed* *idle-blanked* *idle-lid-closed*)
    (setf *idle-dimmed* t)
    ;; Only when the remembered level is higher; that level stays.
    (%brightness "dim" *idle-dim-level*)
    (log-string :info "Idle: backlight dimmed")))

(defun idle-undim ()
  (when *idle-dimmed*
    (setf *idle-dimmed* nil)
    (unless *idle-lid-closed*
      (%brightness "undim"))
    (log-string :info "Idle: backlight restored")))

;;; --- blanking ------------------------------------------------------------

(defun %idle-suspend-views (all)
  "With ALL, mark every window suspended; otherwise suspend exactly the
windows that are not on the panel."
  (loop for group across (state-groups *compositor-state*)
        do (if (or all (not (eq group (state-current-group *compositor-state*))))
               (group-suspend-views group)
               (group-resume-views group))))

(defun idle-blank ()
  (unless *idle-blanked*
    (setf *idle-blanked* t)
    (dolist (output (idle-outputs))
      (hrt:output-set-power output nil))
    ;; Put the remembered level back while the panel is off: i915 keeps it
    ;; for the next power-on, so the panel does not come back at the dim
    ;; level and then jump.
    (when *idle-dimmed*
      (setf *idle-dimmed* nil)
      (%brightness "undim"))
    (%idle-suspend-views t)
    (%sh (format nil "pkill -STOP -f '~A'" *idle-statusbard-pattern*))
    (log-string :info "Idle: panel off")))

(defun %idle-output-change ()
  "Outputs configured while the panel is off (the lid was closed when the
session started, or a monitor was plugged in) must stay off too."
  (when *idle-blanked*
    (dolist (output (idle-outputs))
      (hrt:output-set-power output nil))))

(pushnew '%idle-output-change *output-change-hook*)

(defun idle-unblank ()
  (when *idle-blanked*
    (setf *idle-blanked* nil)
    ;; USR2 makes the sampler publish a fresh line (clock, battery) at once.
    (%sh (format nil "pkill -CONT -f '~A'; pkill -USR2 -f '~A'"
                 *idle-statusbard-pattern* *idle-statusbard-pattern*))
    (dolist (output (idle-outputs))
      (hrt:output-set-power output t))
    ;; Once more now that the panel is on, in case i915 was asleep for the
    ;; level set while it was off.  An unchanged level changes nothing.
    (unless *idle-lid-closed*
      (%brightness "restore"))
    (%idle-suspend-views nil)
    (when (fboundp 'refresh-wake)
      (funcall 'refresh-wake))
    (log-string :info "Idle: panel on")))

;;; --- scheduling ----------------------------------------------------------

(defun idle-schedule ()
  "Arm the timer for the next deadline, and the input callback while the
panel is dimmed or off."
  (when *idle-timer*
    (let ((elapsed (%idle-elapsed-ms))
          (next nil)
          (overdue nil))
      (flet ((consider (seconds done)
               (when (and seconds (not done))
                 (let ((left (- (* 1000 seconds) elapsed)))
                   (if (plusp left)
                       (setf next (if next (min next left) left))
                       (setf overdue t))))))
        (consider *idle-dim-seconds* (or *idle-dimmed* *idle-blanked*))
        (consider *idle-blank-seconds* *idle-blanked*))
      (when (or *idle-dimmed* *idle-blanked*)
        (hrt:hrt-arm-activity-callback))
      ;; An overdue deadline means an inhibitor held it off: look again later.
      (when overdue
        (setf next (if next (min next *idle-inhibited-recheck-ms*)
                       *idle-inhibited-recheck-ms*)))
      (hrt:timer-handle-update *idle-timer* (if next (+ next 200) 0)))))

(defun %idle-tick (timer)
  (declare (ignore timer))
  (let ((elapsed (%idle-elapsed-ms)))
    (unless (hrt:idle-inhibited-p)
      (when (and *idle-dim-seconds* (>= elapsed (* 1000 *idle-dim-seconds*)))
        (idle-dim))
      (when (and *idle-blank-seconds* (>= elapsed (* 1000 *idle-blank-seconds*)))
        (idle-blank))))
  (idle-schedule))

(defun idle-activity ()
  "Input arrived while the panel was dimmed or off."
  (setf *idle-baseline-ms* 0)
  (idle-undim)
  (unless *idle-lid-closed*
    (idle-unblank))
  (idle-schedule))

(hrt:define-hrt-callback handle-input-activity :void () ()
  (idle-activity))

(defun idle-poke ()
  "Activity the kernel's input counter does not see, such as the ThinkPad
brightness keys, which arrive through ACPI: undo dimming and blanking and
count the deadlines from now. Called by the powerd brightness actions."
  (setf *idle-baseline-ms* (or (idle-ms) 0))
  (idle-undim)
  (unless *idle-lid-closed*
    (idle-unblank))
  (idle-schedule)
  :poked)

(defun idle-start ()
  "Start idle management. Safe to call again."
  (unless *idle-timer*
    (setf *idle-baseline-ms* (or (idle-ms) 0))
    (setf *idle-timer* (hrt:server-make-timer (state-server *compositor-state*)
                                              #'%idle-tick))
    (hrt:hrt-set-activity-callback (cffi:callback handle-input-activity)))
  (idle-schedule))

;;; --- lid and manual control ------------------------------------------------

(defun lid-closed ()
  "The lid closed: panel off until it opens. Called by the powerd lid script."
  (setf *idle-lid-closed* t)
  (idle-blank)
  (idle-schedule)
  :panel-off)

(defun lid-opened ()
  (setf *idle-lid-closed* nil
        *idle-baseline-ms* 0)
  (idle-undim)
  (idle-unblank)
  (idle-schedule)
  :panel-on)

(defcommand monitors-off ()
  (:documentation "Turn the panel off now; any input turns it back on")
  (:method ()
    (idle-blank)
    (idle-schedule)))

(defcommand monitors-on ()
  (:documentation "Turn the panel on")
  (:method ()
    (idle-undim)
    (idle-unblank)
    (idle-schedule)))
