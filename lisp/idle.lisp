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

(defvar *idle-hold-hook* nil
  "Functions of no arguments; while one returns true the panel is neither
dimmed nor blanked (a mirror shown to an audience, lisp/mirror.lisp).")
(defvar *idle-timer* nil)
(defvar *idle-dimmed* nil)
(defvar *idle-blanked* nil)
(defvar *idle-power-transition* nil)
(defvar *idle-partial-off* nil) ; failed off and rollback; arm the wake callback
(defvar *idle-restore-pending* nil)
(defvar *idle-lid-file* "/var/run/lid-closed"
  "Exists exactly while the lid is shut: the powerd lid script creates it on
close and removes it on open, in suspend and in nosuspend mode alike.")
(defvar *idle-baseline-ms* 0
  "The idle counter at session start. Deadlines count from here until the
first input event resets the counter, so a session started after a long
idle stretch does not dim or blank at once.")

(defvar *idle-due-ms* nil
  "Monotonic ms at which the armed idle timer should fire, or NIL.")

(defvar *idle-suspend-late-ms* 5000
  "Without *idle-resume-sysctl* (an older kernel): a timer this late is
taken to mean userland was frozen (s2idle).")

(defvar *idle-resume-sysctl* "hw.acpi.sleep.freeze_resumes"
  "Counts the times the kernel thawed userland after an s2idle freeze.")
(defvar *idle-resumes* nil
  "*idle-resume-sysctl* at the last look, or NIL without one.")

(defun %idle-now-ms ()
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun %sh (command)
  "Run COMMAND with /bin/sh in the background."
  (uiop:launch-program (list "/bin/sh" "-c" command)
                       :output nil :error-output nil))

(defun idle-ms ()
  "Milliseconds since the last input event, or NIL when unavailable."
  (%sysctl-integer *idle-sysctl*))

(defun %sysctl-integer (name)
  "The integer sysctl NAME, or NIL when unavailable."
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
                   name (sb-alien:addr value) (sb-alien:addr len)
                   nil 0)))
          (when (zerop rc)
            (if (= len 4) (ldb (byte 32 0) value) value))))
    (error () nil))
  #-sbcl nil)

(defun %idle-elapsed-ms ()
  "Milliseconds of idleness that count towards the deadlines: the kernel's
counter, or the time since the compositor last saw input if that is shorter.
The kernel counts only wskbd/wsmouse events; the virtual pointer and keyboard
of computer use (mhcu) reach only the compositor, and an agent driving the
desktop must not have the panel dimmed and blanked under it every few
seconds."
  (let* ((ms (idle-ms))
         (kernel (cond ((null ms) 0)
                       ((< ms *idle-baseline-ms*) (setf *idle-baseline-ms* 0) ms)
                       (t (- ms *idle-baseline-ms*))))
         (seat (hrt:hrt-ms-since-activity)))
    (if (and ms (<= 0 seat)) (min kernel seat) kernel)))

(defun idle-outputs ()
  (loop for container across (state-cur-outputs *compositor-state*)
        collect (tree:output-container-output container)))

;;; --- dimming -------------------------------------------------------------

(defun %brightness (&rest args)
  "Wait for the script before changing state or enabling an output."
  (handler-case
      (let ((status (nth-value 2 (uiop:run-program
                                  (cons *idle-brightness* (mapcar #'princ-to-string args))
                                  :ignore-error-status t :output nil :error-output nil))))
        (unless (eql status 0)
          (log-string :warn "Idle: brightness ~A returned ~A" (first args) status))
        (eql status 0))
    (error (e) (log-string :warn "Idle: brightness ~A: ~A" (first args) e) nil)))

(defun idle-dim ()
  (unless (or *idle-dimmed* *idle-blanked* (idle-lid-closed-p))
    (when (%brightness "dim" *idle-dim-level*)
      (setf *idle-dimmed* t *idle-restore-pending* nil)
      (log-string :info "Idle: backlight dimmed"))))

(defun idle-undim ()
  (when (and *idle-dimmed* (not (idle-lid-closed-p)) (%brightness "undim"))
    (setf *idle-dimmed* nil)
    (log-string :info "Idle: backlight restored")))

;;; --- blanking ------------------------------------------------------------

(defun %idle-suspend-views (all)
  "With ALL, mark every window suspended; otherwise suspend exactly the
windows that are not on the panel."
  (loop for group across (state-groups *compositor-state*)
        do (if (or all (not (eq group (state-current-group *compositor-state*))))
               (group-suspend-views group)
               (group-resume-views group))))

(defun %idle-power (output on)
  (handler-case (hrt:output-set-power output on)
    (error (e) (log-string :warn "Idle: output power: ~A" e) nil)))

(defun %idle-power-off ()
  ;; Skip outputs already off: rolling those back would turn them on.
  (let ((changed nil))
    (dolist (output (idle-outputs) t)
      (when (hrt:output-enabled output)
        (if (%idle-power output nil)
            (push output changed)
            (progn
              (dolist (other changed)
                (unless (%idle-power other t) (setf *idle-partial-off* t)))
              (return nil)))))))

(defun idle-blank ()
  (unless (or *idle-blanked* *idle-power-transition*)
    (let ((*idle-power-transition* t))
      (when (%idle-power-off)
        (setf *idle-blanked* t *idle-partial-off* nil)
        ;; Only restore after every output is dark. Retain the dim marker
        ;; on failure, so the wake path requests undim rather than restore.
        (when (and *idle-dimmed* (%brightness "undim"))
          (setf *idle-dimmed* nil))
        (%idle-suspend-views t)
        (%sh (format nil "pkill -STOP -f '~A'" *idle-statusbard-pattern*))
        (log-string :info "Idle: panel off")
        t))))

(defun %idle-output-change ()
  (when (and *idle-blanked* (not *idle-power-transition*))
    (let ((*idle-power-transition* t))
      (%idle-power-off))))

(pushnew '%idle-output-change *output-change-hook*)

(defun idle-unblank ()
  (when (and (or *idle-blanked* *idle-partial-off* *idle-restore-pending*)
             (not *idle-power-transition*) (not (idle-lid-closed-p)))
    (let ((*idle-power-transition* t))
      ;; Backlight failure is retryable. It must not strand the user in DPMS off.
      (setf *idle-restore-pending*
            (not (%brightness (if *idle-dimmed* "undim" "restore"))))
      (unless *idle-restore-pending* (setf *idle-dimmed* nil))
      (let ((ok t))
        (dolist (output (idle-outputs))
          (unless (%idle-power output t) (setf ok nil)))
        (when *idle-blanked*
          (%sh (format nil "pkill -CONT -f '~A'; pkill -USR2 -f '~A'"
                       *idle-statusbard-pattern* *idle-statusbard-pattern*))
          (%idle-suspend-views nil)
          (when (fboundp 'refresh-wake) (funcall 'refresh-wake)))
        (setf *idle-blanked* nil *idle-partial-off* (not ok))
        (when ok (log-string :info "Idle: panel on"))
        ok))))

(defun %idle-wake ()
  (if (or *idle-blanked* *idle-partial-off* *idle-restore-pending*)
      (idle-unblank)
      (idle-undim)))

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
      (when (or *idle-dimmed* *idle-blanked* *idle-partial-off* *idle-restore-pending*)
        (hrt:hrt-arm-activity-callback))
      ;; An overdue deadline means an inhibitor held it off: look again later.
      (when overdue
        (setf next (if next (min next *idle-inhibited-recheck-ms*)
                       *idle-inhibited-recheck-ms*)))
      (setf *idle-due-ms* (and next (+ (%idle-now-ms) next 200)))
      (hrt:timer-handle-update *idle-timer* (if next (+ next 200) 0)))))

(defun %idle-tick (timer)
  (declare (ignore timer))
  ;; The kernel's input-idle counter runs on through an s2idle suspension,
  ;; so after a long one every deadline is overdue and the panel would go
  ;; off right after the wake: count the deadlines from the wake instead.
  ;; The kernel counts its thaws; a timer that is merely late (a long
  ;; operation, a stall) is no suspension and its deadline is just due.
  (let ((resumes (%sysctl-integer *idle-resume-sysctl*))
        (late (and *idle-due-ms* (- (%idle-now-ms) *idle-due-ms*))))
    (cond (resumes
           (when (and *idle-resumes* (/= resumes *idle-resumes*))
             (setf *idle-baseline-ms* (or (idle-ms) 0))
             (log-string :info "Idle: userland was frozen (s2idle): deadlines count from now"))
           (setf *idle-resumes* resumes))
          ((and late (> late *idle-suspend-late-ms*))
           (setf *idle-baseline-ms* (or (idle-ms) 0))
           (log-string :info "Idle: timer ~D s late, taken for a suspension: deadlines count from now"
                       (floor late 1000)))))
  (let ((elapsed (%idle-elapsed-ms)))
    (unless (or (hrt:idle-inhibited-p)
                (some #'funcall *idle-hold-hook*))
      (cond ((and *idle-blank-seconds* (>= elapsed (* 1000 *idle-blank-seconds*)))
             (idle-blank))
            ((and *idle-dim-seconds* (>= elapsed (* 1000 *idle-dim-seconds*)))
             (idle-dim)))))
  (idle-schedule))

(defun idle-activity ()
  "Input arrived while the panel was dimmed or off, or while reading mode
had armed the callback (any input, pointer motion included)."
  (when (fboundp 'refresh-note-activity)
    (funcall 'refresh-note-activity))
  (setf *idle-baseline-ms* 0)
  (%idle-wake)
  (idle-schedule))

(hrt:define-hrt-callback handle-input-activity :void () ()
  (idle-activity))

(defun idle-poke ()
  "Activity the kernel's input counter does not see, such as the ThinkPad
brightness keys, which arrive through ACPI: undo dimming and blanking and
count the deadlines from now. Called by the powerd brightness actions."
  (setf *idle-baseline-ms* (or (idle-ms) 0))
  (%idle-wake)
  (idle-schedule)
  :poked)

(defun idle-start ()
  "Start idle management. Safe to call again."
  (unless *idle-timer*
    (setf *idle-baseline-ms* (or (idle-ms) 0)
          *idle-resumes* (%sysctl-integer *idle-resume-sysctl*))
    (setf *idle-timer* (hrt:server-make-timer (state-server *compositor-state*)
                                              #'%idle-tick))
    (hrt:hrt-set-activity-callback (cffi:callback handle-input-activity)))
  (idle-schedule))

;;; --- lid and manual control ------------------------------------------------

(defun idle-lid-closed-p ()
  "Is the lid shut?  Read from the lid script's file every time.  A cached
flag set by (lid-closed) once disagreed with the hardware (a stray call
through the control socket, lid open): every key press was then ignored and
the panel stayed off until a reboot (2026-10-15 and 16)."
  (and (probe-file *idle-lid-file*) t))

(defun lid-closed ()
  "The lid closed: panel off. Called by the powerd lid script after it
created the lid file, which keeps input from turning the panel back on.
Called while the lid is open it acts like monitors-off."
  (idle-blank)
  (idle-schedule)
  (if *idle-blanked* :panel-off :power-failed))

(defun lid-opened ()
  ;; Count from the opening, not from the last key press before the lid
  ;; was shut: the time it was closed is not idleness.
  (setf *idle-baseline-ms* (or (idle-ms) 0))
  (%idle-wake)
  (idle-schedule)
  (if (or *idle-partial-off* *idle-blanked*) :power-failed :panel-on))

(defcommand monitors-off ()
  (:documentation "Turn the panel off now; any input turns it back on")
  (:method ()
    (idle-blank)
    (idle-schedule)))

(defcommand monitors-on ()
  (:documentation "Turn the panel on")
  (:method ()
    (%idle-wake)
    (idle-schedule)))
