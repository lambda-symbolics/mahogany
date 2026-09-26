;;;; Scrolling columns, the way Niri lays windows out: every output holds one
;;;; strip of columns, each column a vertical stack of cells, and the strip
;;;; scrolls horizontally so the focused column is always on the panel.
;;;;
;;;; A strip-frame replaces the binary frame tree as the single child of an
;;;; output node. Its children are the cells, in column order, so the generic
;;;; leaf walkers (foreach-leaf, find-focused-frame ...) keep working. Cells
;;;; that fall outside the viewport have their scene nodes disabled, which is
;;;; what makes wlroots stop sending them frame callbacks: an offscreen client
;;;; does no work at all.
(in-package :mahogany/tree)

(defparameter *strip-gap* 16
  "Pixels between columns, between stacked cells, and around the strip.")

;;; Widths are exact fractions of one row, the way Niri states them: a set of
;;; fractions adding up to 1 tiles the row precisely, gaps included. Ratios,
;;; not floats, so repeated arithmetic stays exact.
(defparameter *strip-default-width* 1/2)
(defparameter *strip-width-presets* '(1/3 1/2 2/3))
(defparameter *strip-width-step* 1/24
  "One press of the widen or narrow key. Twentyfourths, because halves,
thirds, quarters, sixths, eighths and twelfths are all whole numbers of them.")
(defparameter *strip-min-column-px* 80)

(defvar *strip-layout-hook* nil
  "Functions called with the strip after every layout pass.")

(defvar *view-focus-hook* nil
  "Functions called with the hrt:view that just received keyboard focus.")

(defstruct (strip-column (:constructor make-strip-column
                             (&key cells (width *strip-default-width*))))
  (cells nil :type list)
  (width *strip-default-width* :type rational)
  (saved-width nil :type (or null rational)))

(defclass strip-cell (view-frame)
  ((weight :initform 1.0 :accessor cell-weight
           :documentation "Share of the column height, relative to the other cells.")
   (shown :initform t :accessor cell-shown
          :documentation "Whether the cell is currently displayed. Scene nodes
start enabled, so a fresh cell counts as shown until the layout hides it."))
  (:documentation "One window slot inside a strip column."))

(defclass strip-frame (tree-frame)
  ((columns :initform nil :accessor strip-columns)
   (offset :initform 0 :accessor strip-offset
           :documentation "Horizontal scroll position of the viewport, in pixels.")
   (reserved-top :initform 0 :accessor strip-reserved-top
                 :documentation "Pixels kept free at the top, for the bar.")
   (reserved-bottom :initform 0 :accessor strip-reserved-bottom)
   (selected :initform nil :accessor strip-selected
             :documentation "The cell the viewport keeps in view.")
   (layout-depth :initform 0 :accessor %strip-layout-depth))
  (:default-initargs :split-direction :horizontal)
  (:documentation "The scrolling row of columns that fills an output."))

;;; --- structure ----------------------------------------------------------

(defun strip-cells (strip)
  "All cells, column by column, top to bottom."
  (loop for column in (strip-columns strip)
        append (strip-column-cells column)))

(defun cell-column (strip cell)
  (find cell (strip-columns strip) :key #'strip-column-cells :test #'member))

(defun strip-cell-fully-visible-p (cell)
  "Whether CELL is shown and its whole width lies inside the viewport."
  (let ((strip (frame-parent cell)))
    (and (typep strip 'strip-frame)
         (cell-shown cell)
         (multiple-value-bind (left top width height) (strip-area strip)
           (declare (ignore top height))
           (let ((x (round (frame-x cell))))
             (and (>= x (1- left))
                  (<= (+ x (round (frame-width cell))) (+ left width 1))))))))

(defun strip-selected-column (strip)
  (alexandria:when-let ((cell (strip-selected strip)))
    (cell-column strip cell)))

(defun %insert-after (item after items)
  (if (and after (member after items))
      (loop for old in items
            append (if (eq old after) (list old item) (list old)))
      (append items (list item))))

(defun %strip-relink (strip)
  "Refresh the children list and the circular prev/next ring from the columns."
  (let ((cells (strip-cells strip)))
    (setf (tree-children strip) cells)
    (when cells
      (loop for (a b) on cells
            do (setf (%frame-next a) (or b (first cells))
                     (%frame-prev (or b (first cells))) a)))
    cells))

;; An empty strip stands in for its own first and last cell so the ring walkers
;; (find-first-leaf, tree-output-add) have something to hold on to.
(defmethod frame-prev ((strip strip-frame))
  (if (tree-children strip) (call-next-method) strip))

(defmethod frame-next ((strip strip-frame))
  (if (tree-children strip) (call-next-method) strip))

(defmethod (setf %frame-prev) (prev (strip strip-frame))
  (when (tree-children strip) (call-next-method))
  prev)

(defmethod (setf %frame-next) (next (strip strip-frame))
  (when (tree-children strip) (call-next-method))
  next)

(defmethod frame-surface ((strip strip-frame))
  nil)

(defmethod find-empty-frame ((strip strip-frame))
  nil)

(defmethod split-frame-h ((cell strip-cell) &key ratio direction)
  (declare (ignore ratio direction))
  (error 'invalid-operation :text "Strip cells are not split; open a window instead."))

(defmethod split-frame-v ((cell strip-cell) &key ratio direction)
  (declare (ignore ratio direction))
  (error 'invalid-operation :text "Strip cells are not split; use consume instead."))

(defmethod frame-at ((strip strip-frame) x y)
  (declare (type real x y))
  (dolist (cell (strip-cells strip))
    (when (and (cell-shown cell) (in-frame-p cell x y))
      (return cell))))

(defun tree-output-add-strip (layer-container output-container
                              &aux (output (output-container-output output-container)))
  "Add an output node holding an empty strip to LAYER-CONTAINER.
Returns (values output-node strip)."
  (declare (type layer-container layer-container)
           (type output-container output-container))
  (multiple-value-bind (x y) (hrt:output-position output)
    (multiple-value-bind (width height) (hrt:output-resolution output)
      (let* ((node (make-instance 'output-node :parent layer-container
                                               :output output-container))
             (strip (make-instance 'strip-frame :x x :y y
                                                :width width :height height
                                                :parent node)))
        (push node (tree-children layer-container))
        (push strip (tree-children node))
        (values node strip)))))

;;; --- geometry -----------------------------------------------------------

;; The tree-frame methods scale children proportionally on every geometry
;; change. A strip lays its columns out from scratch instead, so bypass them.
(defmethod set-dimensions :around ((strip strip-frame) width height)
  (setf (slot-value strip 'width) width
        (slot-value strip 'height) height)
  (strip-layout strip))

(defmethod set-position :around ((strip strip-frame) x y)
  (setf (slot-value strip 'x) x
        (slot-value strip 'y) y)
  (strip-layout strip))

(defmethod (setf frame-x) :around (new-x (strip strip-frame))
  (setf (slot-value strip 'x) new-x)
  (strip-layout strip)
  new-x)

(defmethod (setf frame-y) :around (new-y (strip strip-frame))
  (setf (slot-value strip 'y) new-y)
  (strip-layout strip)
  new-y)

(defmethod (setf frame-width) :around (new-width (strip strip-frame))
  (setf (slot-value strip 'width) new-width)
  (strip-layout strip)
  new-width)

(defmethod (setf frame-height) :around (new-height (strip strip-frame))
  (setf (slot-value strip 'height) new-height)
  (strip-layout strip)
  new-height)

(defun strip-area (strip)
  "The rectangle columns are laid into: the strip minus gaps and reservations."
  (let ((gap *strip-gap*))
    (values (+ (round (frame-x strip)) gap)
            (+ (round (frame-y strip)) gap (strip-reserved-top strip))
            (max 1 (- (round (frame-width strip)) (* 2 gap)))
            (max 1 (- (round (frame-height strip)) (* 2 gap)
                      (strip-reserved-top strip) (strip-reserved-bottom strip))))))

(defun strip-pixel-width (column width)
  "Pixels for COLUMN's fraction of a row spanning WIDTH between the outer gaps.
Each column is charged one gap and the fraction pays back the one the row does
not have, so fractions summing to 1 tile the row exactly."
  (max *strip-min-column-px*
       (- (floor (* (strip-column-width column) (+ width *strip-gap*)))
          *strip-gap*)))

(defun %cell-show (cell)
  (unless (cell-shown cell)
    (setf (cell-shown cell) t)
    (alexandria:when-let ((view (frame-surface cell)))
      (hrt:view-set-hidden view nil))
    (alexandria:when-let ((box (slot-value cell 'border-box)))
      (hrt:hrt-border-box-set-enabled box t))))

(defun %cell-hide (cell)
  (when (cell-shown cell)
    (setf (cell-shown cell) nil)
    (alexandria:when-let ((view (frame-surface cell)))
      (hrt:view-set-hidden view t))
    (alexandria:when-let ((box (slot-value cell 'border-box)))
      (hrt:hrt-border-box-set-enabled box nil))))

(defun strip-fullscreen-view (strip)
  "The view fullscreened on this strip's output, or NIL."
  (let ((node (frame-parent strip)))
    (and (typep node 'output-node)
         (alexandria:when-let ((data (output-node-fullscreen node)))
           (%fullscreen-data-view data)))))

(defun strip-layout (strip &key center)
  "Place every cell: reveal the selected column, position the visible cells and
hide the ones that fall outside the viewport."
  (declare (type strip-frame strip))
  (when (plusp (%strip-layout-depth strip))
    (return-from strip-layout nil))
  (incf (%strip-layout-depth strip))
  (unwind-protect
       (hrt:with-view-transaction ()
         (multiple-value-bind (left top width height) (strip-area strip)
           (let* ((selected (strip-selected strip))
                  (selected-column (and selected (cell-column strip selected)))
                  (fullscreen (strip-fullscreen-view strip))
                  (start 0) (selected-start 0) (selected-width width))
             (unless selected-column
               (setf selected-column (first (strip-columns strip))
                     selected (first (and selected-column
                                          (strip-column-cells selected-column)))
                     (strip-selected strip) selected))
             (dolist (column (strip-columns strip))
               (when (eq column selected-column)
                 (setf selected-start start
                       selected-width (strip-pixel-width column width)))
               (incf start (+ (strip-pixel-width column width) *strip-gap*)))
             (when selected-column
               (setf (strip-offset strip)
                     (cond (center (- selected-start (floor (- width selected-width) 2)))
                           ((< selected-start (strip-offset strip)) selected-start)
                           ((> (+ selected-start selected-width)
                               (+ (strip-offset strip) width))
                            (- (+ selected-start selected-width) width))
                           (t (strip-offset strip)))))
             (let ((x (- left (strip-offset strip))))
               (dolist (column (strip-columns strip))
                 (let* ((cw (strip-pixel-width column width))
                        (cells (strip-column-cells column))
                        (available (- height (* *strip-gap* (1- (length cells)))))
                        (total (loop for c in cells sum (cell-weight c)))
                        (y top))
                   (loop for remaining on cells
                         for cell = (car remaining)
                         for last = (null (cdr remaining))
                         for ch = (if last
                                      (- (+ top height) y)
                                      (round (* available (/ (cell-weight cell) total))))
                         for view = (frame-surface cell)
                         for visible = (if fullscreen
                                           (eq view fullscreen)
                                           (and (< x (+ left width)) (> (+ x cw) left)))
                         do (cond
                              ((and visible view (eq view fullscreen))
                               ;; The fullscreen node owns the view's geometry.
                               (%cell-show cell))
                              (visible
                               (set-position cell x y)
                               (set-dimensions cell (max 1 cw) (max 1 ch))
                               (%cell-show cell))
                              (t (%cell-hide cell)))
                            (incf y (+ ch *strip-gap*)))
                   (incf x (+ cw *strip-gap*))))))))
    (decf (%strip-layout-depth strip)))
  (dolist (fn *strip-layout-hook*)
    (funcall fn strip))
  strip)

;;; --- cells in and out ---------------------------------------------------

(defun strip-add-view (strip view &key (after (strip-selected-column strip))
                                       (width *strip-default-width*))
  "Give VIEW a new column right after column AFTER (the selected one by
default, the end when there is none). Returns the new cell."
  (declare (type strip-frame strip))
  (multiple-value-bind (left top width-px height) (strip-area strip)
    (declare (ignore left top))
    (let* ((column (make-strip-column :width width))
           (cell (make-instance 'strip-cell
                                :parent strip
                                :x -100000 :y -100000
                                :width (strip-pixel-width column width-px)
                                :height height)))
      (setf (strip-column-cells column) (list cell)
            (strip-columns strip) (%insert-after column after (strip-columns strip)))
      (%strip-relink strip)
      ;; The cell starts hidden and off-panel; the layout reveals it once it is
      ;; selected. Setting the surface sizes the view for its column.
      (setf (frame-surface cell) view)
      (strip-layout strip)
      cell)))

(defun strip-remove-cell (strip cell)
  "Take CELL out of the strip and release it. Returns the cell that should
take the focus: the first cell of the column now at the removed column's
place, or the strip itself when it is empty."
  (declare (type strip-frame strip) (type strip-cell cell))
  (let* ((columns (strip-columns strip))
         (column (cell-column strip cell))
         (index (or (position column columns) 0)))
    (when column
      (setf (strip-column-cells column)
            (remove cell (strip-column-cells column))))
    (setf (strip-columns strip)
          (remove-if-not #'strip-column-cells (strip-columns strip)))
    (%strip-relink strip)
    (when (eq (strip-selected strip) cell)
      (setf (strip-selected strip) nil))
    ;; The tree-parent :after method on remove-frame-from-parent releases the
    ;; cell (border box, view container), so it is not done here.
    (let* ((columns (strip-columns strip))
           (target (and columns
                        (nth (min index (1- (length columns))) columns))))
      (or (and target (first (strip-column-cells target)))
          strip))))

(defmethod remove-frame-from-parent ((parent strip-frame) (frame strip-cell) cleanup-func)
  (declare (ignore cleanup-func))
  (strip-remove-cell parent frame))

(defmethod remove-frame-from-parent :after ((parent strip-frame) (frame strip-cell) cleanup-func)
  (declare (ignore cleanup-func))
  (strip-layout parent))

(defmethod mark-frame-focused :before ((cell strip-cell) seat)
  (declare (ignore seat))
  (let ((strip (frame-parent cell)))
    (when (typep strip 'strip-frame)
      (setf (strip-selected strip) cell)
      (strip-layout strip))))

(defmethod mark-frame-focused :after ((cell strip-cell) seat)
  (declare (ignore seat))
  (alexandria:when-let ((view (frame-surface cell)))
    (dolist (fn *view-focus-hook*)
      (funcall fn view))))

;;; --- navigation and arrangement -----------------------------------------

(defun strip-focus-target (strip direction)
  "The cell that DIRECTION (:left :right :up :down :first :last) leads to
from the selected cell, or NIL."
  (let* ((columns (strip-columns strip))
         (cell (strip-selected strip))
         (column (and cell (cell-column strip cell)))
         (index (or (position column columns) 0)))
    (flet ((column-head (i)
             (let ((c (nth (max 0 (min i (1- (length columns)))) columns)))
               (and c (first (strip-column-cells c))))))
      (case direction
        (:left (column-head (1- index)))
        (:right (column-head (1+ index)))
        (:first (column-head 0))
        (:last (column-head (1- (length columns))))
        ((:up :down)
         (when column
           (let* ((cells (strip-column-cells column))
                  (i (or (position cell cells) 0))
                  (next (max 0 (min (1- (length cells))
                                    (+ i (if (eq direction :up) -1 1))))))
             (nth next cells))))))))

(defun strip-move (strip direction)
  "Reorder the selected column horizontally or the selected cell vertically."
  (let* ((cell (strip-selected strip))
         (column (and cell (cell-column strip cell))))
    (when column
      (let* ((vertical (member direction '(:up :down)))
             (items (copy-list (if vertical (strip-column-cells column) (strip-columns strip))))
             (item (if vertical cell column))
             (index (position item items))
             (target (case direction
                       (:first 0)
                       (:last (1- (length items)))
                       ((:left :up) (1- index))
                       (t (1+ index)))))
        (setf target (max 0 (min target (1- (length items)))))
        (setf items (remove item items))
        (setf items (append (subseq items 0 target) (list item) (nthcdr target items)))
        (if vertical
            (setf (strip-column-cells column) items)
            (setf (strip-columns strip) items))
        (%strip-relink strip)
        (strip-layout strip)))))

(defun strip-snap-width (width &optional (delta 0))
  "WIDTH moved by DELTA steps and snapped onto the lattice of exact fractions."
  (max *strip-width-step*
       (min 1 (* *strip-width-step* (+ (round width *strip-width-step*) delta)))))

(defun strip-set-width (strip action)
  "ACTION is :preset (cycle the presets), :max (toggle full width), :+ or :-."
  (alexandria:when-let ((column (strip-selected-column strip)))
    (let ((width (strip-column-width column)))
      (setf (strip-column-width column)
            (case action
              (:preset
               (or (find-if (lambda (p) (> p (+ width 1/100))) *strip-width-presets*)
                   (first *strip-width-presets*)))
              (:max
               (if (strip-column-saved-width column)
                   (prog1 (strip-column-saved-width column)
                     (setf (strip-column-saved-width column) nil))
                   (progn (setf (strip-column-saved-width column) width) 1)))
              (:+ (strip-snap-width width 1))
              (t (strip-snap-width width -1))))
      (unless (eq action :max)
        (setf (strip-column-saved-width column) nil)))
    (strip-layout strip)))

(defun strip-adjust-height (strip direction)
  "Move the selected cell's share of its column by five percentage points."
  (let* ((cell (strip-selected strip))
         (column (and cell (cell-column strip cell))))
    (when (and column (cdr (strip-column-cells column)))
      (let* ((cells (strip-column-cells column))
             (total (loop for c in cells sum (cell-weight c)))
             (old (/ (cell-weight cell) total))
             (new (max 0.1 (min 0.9 (+ old (if (eq direction :+) 0.05 -0.05))))))
        (dolist (c cells)
          (setf (cell-weight c)
                (if (eq c cell)
                    new
                    (* (/ (cell-weight c) total) (/ (- 1 new) (- 1 old)))))))
      (strip-layout strip))))

(defun strip-consume (strip)
  "Pull the first cell of the column on the right into the selected column."
  (let* ((column (strip-selected-column strip))
         (next (second (member column (strip-columns strip)))))
    (when (and column next)
      (let ((cell (pop (strip-column-cells next))))
        (setf (strip-column-cells column)
              (append (strip-column-cells column) (list cell)))
        (unless (strip-column-cells next)
          (setf (strip-columns strip) (remove next (strip-columns strip))))
        (%strip-relink strip)
        (strip-layout strip)))))

(defun strip-expel (strip)
  "Move the selected cell out of its stack into a new column on the right."
  (let* ((cell (strip-selected strip))
         (column (and cell (cell-column strip cell))))
    (when (and column (cdr (strip-column-cells column)))
      (setf (strip-column-cells column) (remove cell (strip-column-cells column))
            (strip-columns strip)
            (%insert-after (make-strip-column :cells (list cell)
                                              :width (strip-column-width column))
                           column (strip-columns strip)))
      (%strip-relink strip)
      (strip-layout strip))))

(defun strip-take-column (strip column)
  "Detach COLUMN from STRIP, returning its cells' views with their weights as
an alist and the column width. The cells are released."
  (let ((views (loop for cell in (strip-column-cells column)
                     collect (cons (frame-surface cell) (cell-weight cell))))
        (width (strip-column-width column)))
    (dolist (cell (strip-column-cells column))
      (when (eq (strip-selected strip) cell)
        (setf (strip-selected strip) nil))
      (cleanup-frame cell))
    (setf (strip-columns strip) (remove column (strip-columns strip)))
    (%strip-relink strip)
    (strip-layout strip)
    (values views width)))

(defun strip-add-column (strip views width &key (after (strip-selected-column strip)))
  "Add a column made of VIEWS, an alist of (view . weight), after column AFTER.
Returns the first cell."
  (multiple-value-bind (left top width-px height) (strip-area strip)
    (declare (ignore left top))
    (let* ((column (make-strip-column :width width))
           (cells (loop for (view . weight) in views
                        collect (let ((cell (make-instance 'strip-cell
                                                           :parent strip
                                                           :x -100000 :y -100000
                                                           :width (strip-pixel-width column width-px)
                                                           :height height)))
                                  (setf (cell-weight cell) weight)
                                  cell))))
      (setf (strip-column-cells column) cells
            (strip-columns strip) (%insert-after column after (strip-columns strip)))
      (%strip-relink strip)
      (loop for cell in cells
            for (view . nil) in views
            do (setf (frame-surface cell) view))
      (strip-layout strip)
      (first cells))))

(defmethod print-object ((object strip-frame) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":columns ~A :offset ~A :selected ~S"
            (length (strip-columns object)) (strip-offset object)
            (strip-selected object))))
