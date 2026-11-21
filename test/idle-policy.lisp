;;;; Focused host-only tests for lisp/idle.lisp.
(require :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :hrt)
    (defpackage #:hrt (:use #:cl)
      (:export #:hrt-ms-since-activity #:output-enabled #:output-set-power
               #:hrt-arm-activity-callback #:timer-handle-update
               #:define-hrt-callback #:server-make-timer
               #:hrt-set-activity-callback #:idle-inhibited-p)))
  (unless (find-package :tree)
    (defpackage #:tree (:use #:cl) (:export #:output-container-output)))
  (unless (find-package :cffi)
    (defpackage #:cffi (:use #:cl) (:export #:callback)))
  (unless (find-package :mahogany)
    (defpackage #:mahogany (:use #:cl))))

(in-package #:hrt)
(defvar *power-results* nil)
(defvar *power-calls* nil)
(defvar *during-commit* nil)
(defun output-enabled (output) (declare (ignore output)) t)
(defun output-set-power (output on)
  (push (list output on) *power-calls*)
  (when *during-commit* (funcall *during-commit*))
  (not (eq (pop *power-results*) :fail)))
(defun hrt-ms-since-activity () 0)
(defun hrt-arm-activity-callback () t)
(defun timer-handle-update (&rest ignored) (declare (ignore ignored)) t)
(defun idle-inhibited-p () nil)
(defmacro define-hrt-callback (name result args types &body body)
  (declare (ignore result args types))
  `(defun ,name (&rest ignored) (declare (ignore ignored)) ,@body))
(defun server-make-timer (&rest ignored) (declare (ignore ignored)) 'timer)
(defun hrt-set-activity-callback (&rest ignored) (declare (ignore ignored)) t)

(in-package #:tree)
(defun output-container-output (output) output)

(in-package #:cffi)
(unless (macro-function 'callback)
  (defmacro callback (name) `(function ,name)))

(in-package #:mahogany)
(defvar *compositor-state* 'state)
(defvar *output-change-hook* nil)
(defun state-cur-outputs (state) (declare (ignore state)) (vector :a :b))
(defun state-groups (state) (declare (ignore state)) #())
(defun state-current-group (state) (declare (ignore state)) nil)
(defun state-server (state) (declare (ignore state)) nil)
(defun group-suspend-views (&rest ignored) (declare (ignore ignored)) t)
(defun group-resume-views (&rest ignored) (declare (ignore ignored)) t)
(defun log-string (&rest ignored) (declare (ignore ignored)) t)
(defmacro defcommand (name &rest ignored)
  (declare (ignore ignored)) `(defun ,name (&rest args) (declare (ignore args)) nil))

(load (merge-pathnames "../lisp/idle.lisp"
                       (uiop:pathname-directory-pathname *load-truename*)))

;; Never run the sampler or query a real power device in this test.
(setf (symbol-function (quote %sh)) (constantly nil)
      (symbol-function (quote %sysctl-integer)) (constantly nil))

(defun check (condition format-control &rest args)
  (unless condition (error (apply #'format nil format-control args))))
(defun reset-state ()
  (setf hrt::*power-results* nil
        hrt::*power-calls* nil
        *idle-dimmed* nil
        *idle-blanked* nil
        *idle-partial-off* nil
        *idle-restore-pending* nil
        hrt::*during-commit* nil
        *idle-lid-file* "/definitely/missing"
        *idle-timer* nil))

;; Brightness failure must not claim that the panel is dimmed, and wake failure
;; must leave it blanked so a later activity event can retry.
(reset-state)
(setf *idle-brightness* "/bin/false")
(idle-dim)
(check (not *idle-dimmed*) "failed dim changed state")
(setf *idle-brightness* "/bin/true")
(idle-dim)
(check *idle-dimmed* "successful dim did not change state")
(setf *idle-brightness* "/bin/false")
(idle-undim)
(check *idle-dimmed* "failed undim changed state")

;; A partial output-off transition is rolled back and does not become visible
;; as a blanked state.
(reset-state)
(setf hrt::*power-results* '(nil :fail))
(check (not (idle-blank)) "partial output failure reported success")
(check (not *idle-blanked*) "partial output failure left blanked state")
(check (equal (reverse hrt::*power-calls*)
              '((:a nil) (:b nil) (:a t)))
       "partial output transition was not rolled back: ~S"
       (reverse hrt::*power-calls*))

;; With both deadlines overdue, one tick performs only the blank transition.
(reset-state)
(setf *idle-dim-seconds* 1
      *idle-blank-seconds* 2
      *idle-due-ms* nil)
(setf (symbol-function 'idle-ms) (lambda () 10000)
      *idle-baseline-ms* 0)
(let ((calls nil)
      (old-elapsed (symbol-function '%idle-elapsed-ms))
      (old-dim (symbol-function 'idle-dim))
      (old-blank (symbol-function 'idle-blank)))
  (unwind-protect
       (progn
         (setf (symbol-function '%idle-elapsed-ms) (lambda () 10000)
               (symbol-function 'idle-dim) (lambda () (push :dim calls))
               (symbol-function 'idle-blank) (lambda () (push :blank calls)))
         (%idle-tick nil)
         (check (equal calls '(:blank)) "overdue tick chained transitions: ~S" calls))
    (setf (symbol-function '%idle-elapsed-ms) old-elapsed
          (symbol-function 'idle-dim) old-dim
          (symbol-function 'idle-blank) old-blank)))

;; A failed wake remains retryable; the next activity succeeds.
(reset-state)
(setf *idle-blanked* t
      *idle-brightness* "/bin/true"
      hrt::*power-results* '(:fail))
(check (not (idle-unblank)) "failed wake reported success")
(check *idle-partial-off* "failed wake lost retry state")
(setf hrt::*power-results* '(nil nil))
(idle-activity)
(check (not (or *idle-blanked* *idle-partial-off*)) "activity did not retry wake")

;; A failed blank must never undim a visible panel. A power commit can
;; itself run the output-change hook; it must not start a second transition.
(reset-state)
(let ((events nil) (real (symbol-function '%brightness)))
  (unwind-protect
       (progn
         (setf (symbol-function '%brightness)
               (lambda (op) (push (list op (length hrt::*power-calls*)) events) t)
               *idle-dimmed* t hrt::*power-results* '(nil :fail)
               hrt::*during-commit* #'%idle-output-change)
         (idle-blank)
         (check (and *idle-dimmed* (null events)) "undim before blank")
         (setf hrt::*power-calls* nil)
         (idle-blank)
         (check (equal events '(("undim" 2))) "wrong undim order: ~S" events)
         (idle-activity)
         (check (equal (first events) '("restore" 2)) "restore did not precede wake")
         (check (= (length hrt::*power-calls*) 4) "reentrant power commit"))
    (setf (symbol-function '%brightness) real)))
(reset-state)
(setf *idle-brightness* "/bin/false" *idle-blanked* t *idle-dimmed* t)
(idle-activity)
(check (and (not *idle-blanked*) *idle-dimmed* *idle-restore-pending*)
       "script failure stranded output or lost retry")
(setf *idle-brightness* "/bin/true")
(idle-activity)
(check (not (or *idle-dimmed* *idle-restore-pending*)) "failed retry")
(format t "idle tests: ok~%")
