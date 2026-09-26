;;;; Strip-aware group behaviour. The upstream tiled functions in group.lisp
;;;; are renamed %tiled-...; the functions here keep their public names and
;;;; dispatch on whether the group's output holds a strip.
(in-package #:mahogany)

(defvar *group-layout* :strip
  "Layout given to new outputs in every group: :strip for scrolling columns,
:tiled for the StumpWM-style frame tree.")

(defun output-node-strip (node)
  "The strip of NODE, or NIL when the node holds a frame tree."
  (let ((child (first (tree:tree-children node))))
    (and (typep child 'tree:strip-frame) child)))

(defun group-strip (group)
  "The strip on the group's current output, or NIL."
  (declare (type mahogany-group group))
  (alexandria:when-let ((node (%group-current-output-node group)))
    (output-node-strip node)))

(defun group-strips (group)
  (loop for node being the hash-values of (mahogany-group-output-map group)
        for strip = (output-node-strip node)
        when strip collect strip))

(defun %group-find-cell (group view)
  "The strip cell holding VIEW in GROUP, or NIL."
  (dolist (strip (group-strips group))
    (alexandria:when-let ((cell (tree:find-view-frame strip view)))
      (when (typep cell 'tree:strip-cell)
        (return cell)))))

(defun %seat ()
  (server-seat *compositor-state*))

;;; --- outputs ------------------------------------------------------------

(defun group-add-output (group output-container)
  (if (eq *group-layout* :strip)
      (%strip-group-add-output group output-container)
      (%tiled-group-add-output group output-container)))

(defun %strip-group-add-output (group output-container
                                &aux (output (tree::output-container-output output-container)))
  (declare (type tree:output-container output-container)
           (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
                   (tiled-container mahogany-group-tiled-container)
                   (current-frame mahogany-group-current-frame)
                   (hidden-views mahogany-group-hidden-views))
      group
    (multiple-value-bind (node strip)
        (tree:tree-output-add-strip tiled-container output-container)
      (setf (gethash (hrt:output-full-name output) output-map) node)
      (unless current-frame
        (setf current-frame strip))
      ;; Windows parked while the group had no output get their columns back.
      (loop for view = (%pop-hidden-item hidden-views)
            while view
            do (tree:strip-add-view strip view))
      (log-string :trace "Group map: ~S" output-map)
      node)))

(defun group-add-initialize-view (group view-ptr)
  (declare (type mahogany-group group)
           (type cffi:foreign-pointer view-ptr))
  (alexandria:if-let ((strip (group-strip group)))
    (let* ((tiled-layer (mahogany-group-tiled-container group))
           (hrt-tiled-layer (tree:layer-container-layer tiled-layer))
           (view (hrt:view-init view-ptr)))
      (hrt:scene-layer-add-view hrt-tiled-layer view)
      (push view (mahogany-group-views group))
      ;; First configure: the size of a default column, so the client does
      ;; not paint a full-panel frame that is resized a moment later.
      (multiple-value-bind (left top width height) (tree::strip-area strip)
        (declare (ignore left top))
        (let ((column (tree::make-strip-column)))
          (set-dimensions view
                          (max 1 (- (tree::strip-pixel-width column width)
                                    (* 2 tree::*frame-border-width*)))
                          (max 1 (- height (* 2 tree::*frame-border-width*))))))
      view)
    (%tiled-group-add-initialize-view group view-ptr)))

;;; --- mapping ------------------------------------------------------------

(defun group-map-view (group view)
  (declare (type mahogany-group group)
           (type hrt:view view))
  (alexandria:if-let ((strip (group-strip group)))
    (%strip-map-view group strip view)
    (%tiled-group-map-view group view)))

(defun %strip-map-view (group strip view)
  (hrt:with-view-transaction ()
    (let* ((layer (mahogany-group-tiled-container group))
           (hrt-layer (tree:layer-container-layer layer)))
      (hrt:scene-layer-add-view hrt-layer view))
    ;; A new window ends any fullscreen so it can actually be seen.
    (alexandria:when-let ((fullscreen (tree:strip-fullscreen-view strip)))
      (%strip-unfullscreen group (tree:frame-parent strip) fullscreen))
    (let ((cell (tree:strip-add-view strip view)))
      (group-focus-frame group cell (%seat)))
    (hrt:dirty-view-transaction)))

(defun %strip-view-p (group view)
  "Whether VIEW is managed by a strip in GROUP."
  (let ((container (hrt::view-container view)))
    (or (typep container 'tree:strip-cell)
        (and (typep container 'tree:output-node)
             (output-node-strip container))
        (and (null container) (group-strip group)))))

(defun group-unmap-view (group view)
  (declare (type mahogany-group group))
  (if (%strip-view-p group view)
      (%strip-unmap-view group view)
      (%tiled-group-unmap-view group view)))

(defun %strip-detach-view (group view)
  "Remove VIEW's cell from its strip, moving the focus to the cell that took
its place when it was the focused one. Returns T when a cell was removed."
  (let ((container (hrt::view-container view)))
    (when (typep container 'tree:output-node)
      (%strip-clear-fullscreen group container view))
    (alexandria:when-let ((cell (%group-find-cell group view)))
      (let ((was-current (eq (mahogany-group-current-frame group) cell))
            (next (tree:remove-frame cell)))
        (when was-current
          ;; The old cell is gone; never let group-focus-frame unmark it.
          (setf (mahogany-group-current-frame group) nil)
          (group-focus-frame group next (%seat))))
      t)))

(defun %strip-unmap-view (group view)
  (log-string :trace "unmapping strip view ~S" view)
  (hrt:with-view-transaction ()
    (unless (%strip-detach-view group view)
      (ring-list:remove-item (mahogany-group-hidden-views group) view))
    (hrt:dirty-view-transaction)))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (if (%strip-view-p group view)
      (with-accessors ((view-list mahogany-group-views)
                       (hidden mahogany-group-hidden-views))
          group
        (hrt:with-view-transaction ()
          (unless (%strip-detach-view group view)
            (ring-list:remove-item hidden view)))
        (setf view-list (remove view view-list :test #'equalp)))
      (%tiled-group-remove-view group view)))

(defun group-move-view (source destination view)
  (declare (type mahogany-group destination source))
  (if (or (%strip-view-p source view) (group-strip destination))
      (hrt:with-view-transaction ()
        (if (%strip-view-p source view)
            (progn
              (unless (%strip-detach-view source view)
                (ring-list:remove-item (mahogany-group-hidden-views source) view))
              (setf (mahogany-group-views source)
                    (remove view (mahogany-group-views source) :test #'equalp)))
            (%tiled-group-remove-view source view))
        (tree:layer-container-transfer-view
         (mahogany-group-tiled-container destination) view)
        (push view (mahogany-group-views destination))
        (when (hrt:view-mapped-p view)
          (alexandria:if-let ((strip (group-strip destination)))
            (%strip-adopt-cell destination (tree:strip-add-view strip view))
            (%add-hidden (mahogany-group-hidden-views destination) view))))
      (%tiled-group-move-view source destination view)))

(defun %strip-adopt-cell (group cell)
  "Make CELL the frame GROUP focuses next time it is shown, unless the group
already focuses a window. GROUP is not the current group, so nothing is
focused on the seat here."
  (let ((current (mahogany-group-current-frame group)))
    (when (or (null current) (not (typep current 'tree:strip-cell)))
      (setf (mahogany-group-current-frame group) cell)))
  cell)

;;; --- fullscreen ---------------------------------------------------------

(defun %group-make-fullscreen (group view output)
  (let ((container (hrt::view-container view)))
    (if (typep container 'tree:strip-cell)
        (%strip-make-fullscreen group container view)
        (%tiled-group-make-fullscreen group view output))))

(defun %strip-make-fullscreen (group cell view)
  (hrt:view-set-fullscreen view t)
  (let* ((strip (tree:frame-parent cell))
         (node (tree:frame-parent strip)))
    (tree:unmark-frame-focused cell (%seat))
    (alexandria:when-let ((previous (tree:set-fullscreen node view)))
      (hrt:view-set-fullscreen previous nil)
      (alexandria:when-let ((pcell (%group-find-cell group previous)))
        (setf (hrt::view-container previous) pcell)))
    (setf (tree:strip-selected strip) cell
          (mahogany-group-current-frame group) node)
    (tree:strip-layout strip)
    t))

(defun %strip-clear-fullscreen (group node view)
  "Take VIEW out of NODE's fullscreen slot and back into its cell. Returns the
cell, or NIL when the view has none."
  (hrt:view-set-fullscreen view nil)
  (tree:unmark-frame-focused node (%seat))
  (tree:clear-fullscreen node)
  (alexandria:when-let ((cell (%group-find-cell group view)))
    (setf (hrt::view-container view) cell)
    cell))

(defun %strip-unfullscreen (group node view)
  (let ((cell (%strip-clear-fullscreen group node view))
        (strip (output-node-strip node)))
    (setf (mahogany-group-current-frame group) nil)
    (when cell
      (setf (tree:strip-selected strip) cell))
    (tree:strip-layout strip)
    (group-focus-frame group (or cell strip) (%seat))))

(defun %group-unfullscreen (group view)
  (let ((container (hrt::view-container view)))
    (if (and (typep container 'tree:output-node) (output-node-strip container))
        (%strip-unfullscreen group container view)
        (%tiled-group-unfullscreen group view))))

;;; --- cycling and client requests ----------------------------------------

(defun %strip-cycle-group (group step)
  (let ((cell (mahogany-group-current-frame group)))
    (when (typep cell 'tree:strip-cell)
      (let ((target (funcall step cell)))
        (when (and (typep target 'tree:strip-cell) (not (eq target cell)))
          (group-focus-frame group target (%seat)))))))

(defun group-next-hidden (group)
  (if (group-strip group)
      (%strip-cycle-group group #'tree:frame-next)
      (%tiled-group-next-hidden group)))

(defun group-previous-hidden (group)
  (if (group-strip group)
      (%strip-cycle-group group #'tree:frame-prev)
      (%tiled-group-previous-hidden group)))

(defun group-maximize-view (group view)
  (if (group-strip group)
      ;; Columns have their own width keys; a client asking to be maximized
      ;; only gets its current size confirmed.
      (hrt:view-configure view)
      (%tiled-group-maximize-view group view)))

(defun group-minimize-view (group view)
  (if (group-strip group)
      (hrt:view-configure view)
      (%tiled-group-minimize-view group view)))

(defun group-remove-current-frame (group)
  (if (group-strip group)
      (error 'mahogany/util:invalid-operation
             :text "Strip columns disappear with their last window.")
      (%tiled-group-remove-current-frame group)))
