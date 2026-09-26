;;;; Commands for the scrolling strip layout. Mirrors the StumpWM scrolling
;;;; configuration used on the Nano so the same Super keys keep working.
(in-package #:mahogany)

(defun current-strip ()
  "The strip on the current group's current output, or NIL."
  (group-strip (state-current-group *compositor-state*)))

(defun current-strip-cell ()
  (let ((frame (state-current-frame *compositor-state*)))
    (and (typep frame 'tree:strip-cell) frame)))

(defun %strip-focus (direction)
  (alexandria:when-let* ((strip (current-strip))
                         (target (tree:strip-focus-target strip direction)))
    (state-focus-frame *compositor-state* target (server-seat *compositor-state*))))

(defmacro define-strip-commands (&rest specs)
  "Each spec is (name docstring form)."
  `(progn
     ,@(loop for (name doc form) in specs
             collect `(defcommand ,name ()
                        (:documentation ,doc)
                        (:method () ,form)))))

(define-strip-commands
  (strip-focus-left "Focus the column on the left." (%strip-focus :left))
  (strip-focus-right "Focus the column on the right." (%strip-focus :right))
  (strip-focus-up "Focus the cell above in this column." (%strip-focus :up))
  (strip-focus-down "Focus the cell below in this column." (%strip-focus :down))
  (strip-focus-first "Focus the first column." (%strip-focus :first))
  (strip-focus-last "Focus the last column." (%strip-focus :last))
  (strip-move-left "Move the column one place left." (%strip-move :left))
  (strip-move-right "Move the column one place right." (%strip-move :right))
  (strip-move-up "Move the cell up within its column." (%strip-move :up))
  (strip-move-down "Move the cell down within its column." (%strip-move :down))
  (strip-move-first "Move the column to the first place." (%strip-move :first))
  (strip-move-last "Move the column to the last place." (%strip-move :last))
  (strip-width-preset "Cycle the column width: one third, one half, two thirds."
                      (%strip-width :preset))
  (strip-width-max "Toggle the column between full width and its previous width."
                   (%strip-width :max))
  (strip-width-narrower "Narrow the column by one step." (%strip-width :-))
  (strip-width-wider "Widen the column by one step." (%strip-width :+))
  (strip-height-less "Give the cell a smaller share of its column." (%strip-height :-))
  (strip-height-more "Give the cell a larger share of its column." (%strip-height :+))
  (strip-consume "Pull the first window of the next column into this column."
                 (alexandria:when-let ((strip (current-strip)))
                   (tree:strip-consume strip)))
  (strip-expel "Move this window out of its stack into a new column."
               (alexandria:when-let ((strip (current-strip)))
                 (tree:strip-expel strip)))
  (strip-center "Center the focused column in the viewport."
                (alexandria:when-let ((strip (current-strip)))
                  (tree:strip-layout strip :center t)))
  (strip-fullscreen "Toggle fullscreen on the focused window."
                    (%strip-toggle-fullscreen))
  (strip-next "Focus the next window in the strip." (%strip-cycle #'tree:frame-next))
  (strip-prev "Focus the previous window in the strip." (%strip-cycle #'tree:frame-prev)))

(defun %strip-move (direction)
  (alexandria:when-let ((strip (current-strip)))
    (tree:strip-move strip direction)))

(defun %strip-width (action)
  (alexandria:when-let ((strip (current-strip)))
    (tree:strip-set-width strip action)))

(defun %strip-height (direction)
  (alexandria:when-let ((strip (current-strip)))
    (tree:strip-adjust-height strip direction)))

(defun %strip-cycle (step)
  (let ((frame (state-current-frame *compositor-state*)))
    (when (typep frame 'tree:strip-cell)
      (let ((target (funcall step frame)))
        (when (typep target 'tree:strip-cell)
          (state-focus-frame *compositor-state* target
                             (server-seat *compositor-state*)))))))

(defun %strip-toggle-fullscreen ()
  (let* ((group (state-current-group *compositor-state*))
         (frame (state-current-frame *compositor-state*)))
    (typecase frame
      (tree:output-node
       (alexandria:when-let ((view (tree:frame-surface frame)))
         (group-set-fullscreen group view nil nil)
         (%cur-frame-set-from-group *compositor-state* group)))
      (tree:strip-cell
       (alexandria:when-let ((view (tree:frame-surface frame)))
         (group-set-fullscreen group view nil t)
         (%cur-frame-set-from-group *compositor-state* group))))))

(defun strip-move-column-to-group (destination)
  "Move the focused column, all of its windows, to DESTINATION."
  (let* ((group (state-current-group *compositor-state*))
         (strip (current-strip))
         (column (and strip (tree:strip-selected-column strip))))
    (when (and column (not (eq group destination)))
      (let ((dest-strip (group-strip destination)))
        (unless dest-strip
          (error 'mahogany/util:invalid-operation
                 :text "The destination group has no strip"))
        (multiple-value-bind (views width) (tree:strip-take-column strip column)
          (dolist (entry views)
            (let ((view (car entry)))
              (setf (mahogany-group-views group)
                    (remove view (mahogany-group-views group) :test #'equalp))
              (setf (hrt::view-container view) nil)
              (tree:layer-container-transfer-view
               (mahogany-group-tiled-container destination) view)
              (push view (mahogany-group-views destination))))
          (tree:strip-add-column dest-strip views width))
        ;; The source lost its focused cell; land on whatever took its place.
        (let ((next (or (tree:strip-selected strip)
                        (first (tree:strip-cells strip))
                        strip)))
          (setf (mahogany-group-current-frame group) nil)
          (state-focus-frame *compositor-state* next (server-seat *compositor-state*)))))))

(defcommand strip-workspace
    ((group (:function interactively-read-group :data "Group?")))
  (:documentation "Move the focused column to another group")
  (:method (destination)
    (strip-move-column-to-group destination)))

(defun group-select-by-number (n)
  (state-select-group *compositor-state* n))

(defmacro define-workspace-commands (count)
  `(progn
     ,@(loop for i from 1 to count
             collect `(defcommand ,(intern (format nil "STRIP-WORKSPACE-~D" i)) ()
                        (:documentation ,(format nil "Move the focused column to group ~D" i))
                        (:method ()
                          (alexandria:when-let ((g (%state-select-group *compositor-state* ,i)))
                            (strip-move-column-to-group g)))))))

(define-workspace-commands 10)
