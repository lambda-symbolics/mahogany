;;;; LispBSD: output mirroring, the wl-mirror of this compositor.  A target
;;;; output (a projector on HDMI) shows what a source output (the panel)
;;;; shows.  heart/src/mirror.c does the plumbing without copying: the
;;;; buffer each source frame is committed with goes onto a scene node only
;;;; the target sees, scaled by the GPU.  This file is the policy: which
;;;; outputs, how the picture fits, a region of the source, freezing,
;;;; mirroring a monitor as soon as it is plugged in, and what happens when
;;;; either output goes away.  doc/mirror.org describes it.
(in-package #:mahogany)

(defvar *mirror-fit* :contain
  "How a new mirror fits the source onto the target:
:CONTAIN  all of it, as large as fits, black bars on the sides (default)
:COVER    fill the target, cutting off what sticks out
:STRETCH  fill the target, distorting the aspect ratio
:EXACT    one source pixel per target pixel, centred, cut off if larger")

(defvar *mirror-smooth* t
  "Scale with bilinear filtering; NIL takes the nearest pixel (crisp text at
whole multiples).")

(defvar *mirror-on-connect* nil
  "When true, an output that appears (a monitor plugged in) starts out
mirroring the panel instead of extending the desktop.")

(defvar *mirror-holds-idle* t
  "While a mirror runs, the panel is not dimmed or turned off for idleness:
someone is presenting.")

(defstruct (mirror (:constructor %make-mirror (source target fit)))
  (source nil :type hrt:output)
  (target nil :type hrt:output)
  (handle nil)
  (fit :contain :type keyword)
  ;; (X Y WIDTH HEIGHT) in source layout pixels, NIL for the whole output
  (region nil :type list)
  (frozen nil :type boolean))

(defvar *mirrors* nil
  "The running mirrors.")

(defvar *mirror-outputs-seen* nil
  "The outputs present at the last output change, to tell new ones.")

(defvar *mirror-pending-until* nil
  "While INTERNAL-REAL-TIME is before this, the next output that appears
starts mirroring the panel: MIRROR-TOGGLE asked for a mirror before the
monitor had been found.")

;;; --- outputs ---------------------------------------------------------------

(defun %mirror-all-outputs ()
  (coerce (state-outputs *compositor-state*) 'list))

(defun %mirror-panel ()
  "The built-in panel (eDP, LVDS), else the first output."
  (let ((outputs (%mirror-all-outputs)))
    (or (find-if (lambda (o)
                   (let ((name (hrt:output-name o)))
                     (or (alexandria:starts-with-subseq "eDP" name)
                         (alexandria:starts-with-subseq "LVDS" name))))
                 outputs)
        (first outputs))))

(defun %mirror-find-output (name)
  (or (find name (%mirror-all-outputs) :key #'hrt:output-name :test #'string=)
      (error "mirror: no output called ~S (there are ~{~A~^, ~})" name
             (mapcar #'hrt:output-name (%mirror-all-outputs)))))

(defun mirror-target-p (output)
  "True when OUTPUT is showing a mirror."
  (and (find output *mirrors* :key #'mirror-target :test #'hrt:output=) t))

(defun %mirror-of (target)
  (find target *mirrors* :key #'mirror-target :test #'hrt:output=))

(defun %mirror-default-target (source)
  "The first enabled output that is neither SOURCE nor already a target."
  (find-if (lambda (o)
             (and (not (hrt:output= o source))
                  (not (mirror-target-p o))
                  (hrt:output-enabled o)))
           (%mirror-all-outputs)))

;;; --- geometry ----------------------------------------------------------------

(defun mirror-geometry (fit region-x region-y region-width region-height
                        target-width target-height &optional (exact-scale 1))
  "Where a source region goes on the target.  The region is in source
buffer pixels, the target size in target layout units; EXACT-SCALE is the
target's layout units per pixel (for :EXACT).  Returns the source box
actually shown (X Y WIDTH HEIGHT, pixels) and the box it fills on the target
(X Y WIDTH HEIGHT, from the target's corner), eight values."
  (if (eq fit :stretch)
      (values region-x region-y region-width region-height
              0 0 target-width target-height)
      (let* ((scale (ecase fit
                      (:contain (min (/ target-width region-width)
                                     (/ target-height region-height)))
                      (:cover (max (/ target-width region-width)
                                   (/ target-height region-height)))
                      (:exact exact-scale)))
             ;; The part of the region that fits at that scale, centred.
             (shown-width (min region-width (/ target-width scale)))
             (shown-height (min region-height (/ target-height scale)))
             (width (min target-width (round (* shown-width scale))))
             (height (min target-height (round (* shown-height scale)))))
        (values (+ region-x (/ (- region-width shown-width) 2))
                (+ region-y (/ (- region-height shown-height) 2))
                shown-width shown-height
                (floor (- target-width width) 2)
                (floor (- target-height height) 2)
                width height))))

(defun %mirror-apply-view (mirror)
  "Tell heart where the picture goes, from the outputs' current sizes."
  (let ((source (mirror-source mirror))
        (target (mirror-target mirror)))
    (multiple-value-bind (pixel-width pixel-height) (hrt::output-pixel-size source)
      (multiple-value-bind (source-width source-height) (hrt:output-resolution source)
        (declare (ignore source-height))
        (multiple-value-bind (target-width target-height) (hrt:output-resolution target)
          (when (and (plusp pixel-width) (plusp pixel-height)
                     (plusp source-width) (plusp target-width) (plusp target-height)
                     (plusp (hrt::output-pixel-size target)))
            (let ((to-pixels (/ pixel-width source-width)))
              (destructuring-bind (rx ry rw rh)
                  (if (mirror-region mirror)
                      (mapcar (lambda (v) (* v to-pixels)) (mirror-region mirror))
                      (list 0 0 pixel-width pixel-height))
                ;; Keep the region on the source.
                (let* ((rx (max 0 (min rx (1- pixel-width))))
                       (ry (max 0 (min ry (1- pixel-height))))
                       (rw (max 1 (min rw (- pixel-width rx))))
                       (rh (max 1 (min rh (- pixel-height ry)))))
                  (multiple-value-bind (sx sy sw sh x y w h)
                      (mirror-geometry (mirror-fit mirror) rx ry rw rh
                                       target-width target-height
                                       (/ target-width
                                          (hrt::output-pixel-size target)))
                    (hrt::hrt-mirror-set-view
                     (mirror-handle mirror)
                     (coerce sx 'double-float) (coerce sy 'double-float)
                     (coerce sw 'double-float) (coerce sh 'double-float)
                     x y w h target-width target-height
                     (and *mirror-smooth* t))))))))))))

;;; --- starting and stopping -------------------------------------------------------

(defun mirror-start (&key target source (fit *mirror-fit*) region)
  "Make the output named TARGET (default: the first other output) show the
output named SOURCE (default: the panel).  The target leaves the desktop:
its windows move to the remaining outputs and the pointer cannot enter it."
  (let* ((source (if source (%mirror-find-output source) (%mirror-panel)))
         (target (if target
                     (%mirror-find-output target)
                     (or (%mirror-default-target source)
                         (error "mirror: no other output to mirror ~A onto"
                                (hrt:output-name source))))))
    (cond ((hrt:output= source target)
           (error "mirror: ~A cannot mirror itself" (hrt:output-name source)))
          ((mirror-target-p target)
           (error "mirror: ~A already shows a mirror" (hrt:output-name target)))
          ((mirror-target-p source)
           (error "mirror: ~A shows a mirror itself" (hrt:output-name source))))
    (let ((mirror (%make-mirror source target fit))
          (state *compositor-state*))
      (setf (mirror-region mirror) region)
      ;; Listed first, so nothing gives the target a container again.
      (push mirror *mirrors*)
      (when (%find-output-container (hrt:output-hrt-output target) state)
        (%remove-output-container state target))
      (let ((handle (hrt::hrt-mirror-start (hrt:output-hrt-output source)
                                           (hrt:output-hrt-output target))))
        (when (cffi:null-pointer-p handle)
          (setf *mirrors* (delete mirror *mirrors*))
          (%add-output-container state target)
          (error "mirror: heart refused ~A -> ~A" (hrt:output-name source)
                 (hrt:output-name target)))
        (setf (mirror-handle mirror) handle))
      (%mirror-apply-view mirror)
      (log-string :info "Mirror: ~A shows ~A (~(~A~))" (hrt:output-name target)
                  (hrt:output-name source) fit)
      mirror)))

(defun %mirror-forget (mirror restore)
  (setf *mirrors* (delete mirror *mirrors*))
  (hrt::hrt-mirror-stop (mirror-handle mirror) restore))

(defun mirror-stop (&optional target)
  "Stop the mirror on the output named TARGET (default: every mirror); the
output joins the desktop again."
  (let ((mirrors (if target
                     (list (or (%mirror-of (%mirror-find-output target))
                               (error "mirror: ~A shows no mirror" target)))
                     (copy-list *mirrors*)))
        (state *compositor-state*))
    (dolist (mirror mirrors)
      (let ((output (mirror-target mirror)))
        (%mirror-forget mirror t)
        (when (hrt:output-enabled output)
          (%add-output-container state output))
        (log-string :info "Mirror on ~A stopped" (hrt:output-name output))))
    (when mirrors
      ;; Back where the layout configuration puts it.
      (state-output-layouts-rescan state))
    (length mirrors)))

(defun outputs-rescan ()
  "Look for monitors plugged in or pulled out since the last look.  NetBSD
tells userland nothing about connector changes, so this is how a new monitor
is found; the outputs arrive (and *OUTPUT-CHANGE-HOOK* runs) shortly after."
  (hrt::hrt-outputs-rescan (state-server *compositor-state*)))

(defun mirror-toggle ()
  "Stop every mirror, or mirror the panel onto the first other output.  With
no other output yet, look for one and mirror onto it when it appears."
  (cond (*mirrors*
         (mirror-stop)
         ;; A monitor pulled out while mirroring is noticed now.
         (outputs-rescan))
        ((%mirror-default-target (%mirror-panel))
         (mirror-start))
        (t
         (setf *mirror-pending-until*
               (+ (get-internal-real-time)
                  (* 5 internal-time-units-per-second)))
         (outputs-rescan)
         (log-string :info "Mirror: looking for a monitor"))))

;;; --- changing a running mirror --------------------------------------------------

(defun %mirror-select (target)
  (if target
      (or (%mirror-of (%mirror-find-output target))
          (error "mirror: ~A shows no mirror" target))
      (or (first *mirrors*) (error "mirror: no mirror is running"))))

(defun mirror-set-fit (fit &optional target)
  (check-type fit (member :contain :cover :stretch :exact))
  (let ((mirror (%mirror-select target)))
    (setf (mirror-fit mirror) fit)
    (%mirror-apply-view mirror)
    fit))

(defun mirror-cycle-fit (&optional target)
  (let* ((fits '(:contain :cover :stretch :exact))
         (mirror (%mirror-select target))
         (next (or (second (member (mirror-fit mirror) fits)) (first fits))))
    (mirror-set-fit next target)))

(defun mirror-set-region (region &optional target)
  "Show only REGION, (X Y WIDTH HEIGHT) in the source's layout pixels, or the
whole source for NIL."
  (when region
    (assert (and (= (length region) 4) (every #'realp region)
                 (plusp (third region)) (plusp (fourth region)))
            () "mirror: a region is (X Y WIDTH HEIGHT), got ~S" region))
  (let ((mirror (%mirror-select target)))
    (setf (mirror-region mirror) region)
    (%mirror-apply-view mirror)
    region))

(defun mirror-freeze (frozen &optional target)
  "Keep the target on the current picture (FROZEN true) or follow again."
  (let ((mirror (%mirror-select target)))
    (setf (mirror-frozen mirror) (and frozen t))
    (hrt::hrt-mirror-set-frozen (mirror-handle mirror) (mirror-frozen mirror))
    (mirror-frozen mirror)))

;;; --- hooks ------------------------------------------------------------------------

(defun mirror-capture (path &optional target)
  "Write the next frame the mirror target shows to PATH (a PNG; it appears
when complete).  grim cannot capture a target: it is not in the layout."
  (let ((mirror (%mirror-select target)))
    (ignore-errors (delete-file path))
    (unless (hrt::hrt-mirror-capture (mirror-handle mirror) path)
      (error "mirror: cannot capture ~A" (hrt:output-name (mirror-target mirror))))
    path))

(defun %mirror-output-removed (output)
  "OUTPUT is going away: a mirror on it ends; a mirror of it ends and its
target joins the desktop."
  (dolist (mirror (copy-list *mirrors*))
    (cond ((hrt:output= output (mirror-target mirror))
           (%mirror-forget mirror nil)
           (log-string :info "Mirror target ~A went away" (hrt:output-name output)))
          ((hrt:output= output (mirror-source mirror))
           (%mirror-forget mirror t)
           (%add-output-container *compositor-state* (mirror-target mirror))
           (log-string :info "Mirror source ~A went away" (hrt:output-name output))))))

(defun %mirror-output-change ()
  "After outputs were (re)configured: refit every mirror to the current
modes, and with *MIRROR-ON-CONNECT* mirror the panel onto new outputs."
  (dolist (mirror *mirrors*)
    (%mirror-apply-view mirror))
  (let ((outputs (%mirror-all-outputs))
        (pending (and *mirror-pending-until*
                      (< (get-internal-real-time) *mirror-pending-until*))))
    (setf *mirror-pending-until* nil)
    (when (or *mirror-on-connect* pending)
      (let ((panel (%mirror-panel)))
        (dolist (output outputs)
          (unless (or (member output *mirror-outputs-seen* :test #'hrt:output=)
                      (hrt:output= output panel)
                      (mirror-target-p output)
                      (not (hrt:output-enabled output)))
            (handler-case (mirror-start :target (hrt:output-name output))
              (error (e) (log-string :error "~A" e)))))))
    (setf *mirror-outputs-seen* outputs)))

(defun %mirror-holds-idle-p ()
  (and *mirror-holds-idle* *mirrors* t))

(pushnew 'mirror-target-p *output-excluded-hook*)
(pushnew '%mirror-output-removed *output-removed-hook*)
(pushnew '%mirror-output-change *output-change-hook*)
(pushnew '%mirror-holds-idle-p *idle-hold-hook*)

;;; --- the control socket and keys --------------------------------------------------

(defun %mirror-describe (mirror)
  (list :source (hrt:output-name (mirror-source mirror))
        :target (hrt:output-name (mirror-target mirror))
        :fit (string-downcase (symbol-name (mirror-fit mirror)))
        :region (or (mirror-region mirror) :null)
        :frozen (if (mirror-frozen mirror) :true :false)
        :frames (hrt::hrt-mirror-frames (mirror-handle mirror))))

(defun mirror-status ()
  "JSON: the outputs and the running mirrors."
  (%cu-result
   (list :outputs (mapcar (lambda (o)
                            (multiple-value-bind (w h) (hrt::output-pixel-size o)
                              (list :name (hrt:output-name o)
                                    :enabled (if (hrt:output-enabled o) :true :false)
                                    :width w :height h
                                    :mirror_target (if (mirror-target-p o) :true :false))))
                          (%mirror-all-outputs))
         :mirrors (coerce (mapcar #'%mirror-describe *mirrors*) 'vector))))

(defmacro %mirror-command (&body body)
  "Run BODY for the control socket: the status as JSON, or the error."
  `(handler-case (progn ,@body (mirror-status))
     (error (e) (%cu-result (list :error (princ-to-string e))))))

(defun mirror-command (action &rest args)
  "The control socket's entry point (mhcu mirror ...).  ACTION is a string:
start [TARGET [SOURCE]], stop [TARGET], toggle, fit FIT, region X Y W H,
region (whole source), freeze, thaw, shot PATH [TARGET], rescan, status."
  (let ((action (string-downcase action)))
    (cond ((string= action "status") (mirror-status))
          ((string= action "start")
           (%mirror-command (mirror-start :target (first args) :source (second args))))
          ((string= action "stop") (%mirror-command (mirror-stop (first args))))
          ((string= action "toggle") (%mirror-command (mirror-toggle)))
          ((string= action "rescan") (%mirror-command (outputs-rescan)))
          ((string= action "fit")
           (%mirror-command
            (mirror-set-fit (intern (string-upcase (or (first args) "")) :keyword))))
          ((string= action "region")
           (%mirror-command
            (mirror-set-region (and args (mapcar (lambda (a)
                                                    (if (stringp a) (parse-integer a) a))
                                                  args)))))
          ((string= action "shot")
           (handler-case (%cu-result (list :path (mirror-capture (first args)
                                                                 (second args))))
             (error (e) (%cu-result (list :error (princ-to-string e))))))
          ((string= action "freeze") (%mirror-command (mirror-freeze t)))
          ((string= action "thaw") (%mirror-command (mirror-freeze nil)))
          (t (%cu-result (list :error (format nil "mirror: unknown action ~S" action)))))))

(defcommand mirror-screen ()
  (:documentation "Mirror the panel onto the other output, or stop mirroring")
  (:method ()
    (handler-case (mirror-toggle)
      (error (e) (log-string :error "~A" e)))))

(defcommand outputs-look ()
  (:documentation "Look for monitors plugged in or pulled out")
  (:method ()
    (outputs-rescan)))

(defcommand mirror-next-fit ()
  (:documentation "Cycle how the mirror fits: contain, cover, stretch, exact")
  (:method ()
    (handler-case (mirror-cycle-fit)
      (error (e) (log-string :error "~A" e)))))
