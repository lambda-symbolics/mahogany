;;;; LispBSD: computer use.  Commands a remote agent sends over the control
;;;; socket (control.lisp) to look at and operate the desktop for GUI tests:
;;;; list windows, screenshots, pointer and keyboard.  Input goes through
;;;; heart's virtual devices (heart/src/computer_use.c), so it takes the path
;;;; real input takes, keybindings and focus rules included.  Every command
;;;; returns a JSON string; doc/computer-use.org describes them and the
;;;; mhcu(1) client that wraps them.
(in-package #:mahogany)

;;; --- JSON ---------------------------------------------------------------

(defun %cu-json (value stream)
  "Write VALUE as JSON: strings, integers, reals, T/:true, NIL/:null/:false,
plists with keyword keys as objects (a list starting with a keyword), other
lists and vectors as arrays."
  (cond ((eq value t) (write-string "true" stream))
        ((eq value :true) (write-string "true" stream))
        ((eq value :false) (write-string "false" stream))
        ((or (null value) (eq value :null)) (write-string "null" stream))
        ((stringp value)
         (write-char #\" stream)
         (loop for c across value
               do (case c
                    (#\" (write-string "\\\"" stream))
                    (#\\ (write-string "\\\\" stream))
                    (#\Newline (write-string "\\n" stream))
                    (#\Return (write-string "\\r" stream))
                    (#\Tab (write-string "\\t" stream))
                    (t (if (< (char-code c) 32)
                           (format stream "\\u~4,'0X" (char-code c))
                           (write-char c stream)))))
         (write-char #\" stream))
        ((integerp value) (format stream "~D" value))
        ((realp value) (format stream "~,3F" value))
        ((and (consp value) (keywordp (car value)))
         (write-char #\{ stream)
         (loop for (k v) on value by #'cddr
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (%cu-json (string-downcase (substitute #\_ #\- (symbol-name k)))
                            stream)
                  (write-char #\: stream)
                  (%cu-json v stream))
         (write-char #\} stream))
        ((or (listp value) (vectorp value))
         (write-char #\[ stream)
         (let ((first t))
           (map nil (lambda (v)
                      (unless first (write-char #\, stream))
                      (setf first nil)
                      (%cu-json v stream))
                value))
         (write-char #\] stream))
        (t (%cu-json (princ-to-string value) stream))))

(defun %cu-result (value)
  (with-output-to-string (s) (%cu-json value s)))

(defvar *cu-enabled* t
  "NIL makes every computer-use command refuse: the user's kill switch.")

(defmacro %cu-command (&body body)
  "Run BODY; its value as JSON, or {\"error\": message}."
  `(handler-case
       (if *cu-enabled*
           (%cu-result (progn ,@body))
           (%cu-result (list :error "computer use is disabled (*cu-enabled*)")))
     (error (c) (%cu-result (list :error (princ-to-string c))))))

;;; --- the screen and its windows -----------------------------------------

(defun %cu-seat () (server-seat *compositor-state*))

(defun %cu-layout-box ()
  (cffi:with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
    (hrt::hrt-layout-box (state-server *compositor-state*) x y w h)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int)
            (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun %cu-view-id (view)
  (cffi:pointer-address (hrt:view-hrt-view view)))

(defun %cu-view-group (view)
  (%find-view-in-groups view (state-groups *compositor-state*)))

(defun %cu-window (view)
  (let ((hrt-view (hrt:view-hrt-view view))
        (group (%cu-view-group view)))
    (cffi:with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
      (let ((shown (hrt::hrt-view-geometry hrt-view x y w h)))
        (list :id (%cu-view-id view)
              :app-id (or (hrt::hrt-view-app-id hrt-view) "")
              :title (or (hrt:view-title view) "")
              :pid (or (hrt:view-pid view) :null)
              :group (if group (mahogany-group-number group) :null)
              :group-name (if group (mahogany-group-name group) :null)
              :current-group (if (and group
                                      (eq group (state-current-group
                                                 *compositor-state*)))
                                 :true :false)
              :x (cffi:mem-ref x :int) :y (cffi:mem-ref y :int)
              :width (cffi:mem-ref w :int) :height (cffi:mem-ref h :int)
              :visible (if (and shown (hrt:view-mapped-p view)) :true :false)
              :focused (if (hrt::view-focused-p view) :true :false)
              :fullscreen (if (hrt:view-fullscreen-p view) :true :false))))))

(defun %cu-views ()
  (sort (loop for v being the hash-values of (state-views *compositor-state*)
              when (hrt:view-mapped-p v) collect v)
        #'< :key #'%cu-view-id))

(defun %cu-find-view (id)
  (or (gethash id (state-views *compositor-state*))
      (error "no window with id ~A" id)))

(defun cu-screen ()
  "The layout in pixels, its outputs, the pointer and the current group."
  (%cu-command
    (multiple-value-bind (x y w h) (%cu-layout-box)
      (let ((seat (%cu-seat))
            (group (state-current-group *compositor-state*)))
        (list :x x :y y :width w :height h
              :panel (cond (*idle-blanked* "blanked")
                           (*idle-dimmed* "dimmed")
                           (t "on"))
              :pointer (list (round (hrt:hrt-seat-cursor-lx seat))
                             (round (hrt:hrt-seat-cursor-ly seat)))
              :group (mahogany-group-number group)
              :group-name (mahogany-group-name group)
              :groups (map 'list (lambda (g)
                                   (list :number (mahogany-group-number g)
                                         :name (mahogany-group-name g)))
                           (state-groups *compositor-state*))
              :outputs (map 'list
                            (lambda (o)
                              (multiple-value-bind (ox oy) (hrt::output-position o)
                                (multiple-value-bind (ow oh) (hrt::output-resolution o)
                                  (list :name (hrt::output-full-name o)
                                        :x ox :y oy :width ow :height oh))))
                            (state-outputs *compositor-state*)))))))

(defun cu-windows ()
  "Every mapped window, in every group."
  (%cu-command (or (mapcar #'%cu-window (%cu-views)) #())))

(defun cu-focused ()
  (%cu-command
    (let ((v (find-if #'hrt::view-focused-p (%cu-views))))
      (if v (%cu-window v) :null))))

(defun cu-focus (id)
  "Switch to the window's group and focus it."
  (%cu-command
    (let* ((view (%cu-find-view id))
           (group (or (%cu-view-group view) (error "window ~A has no group" id))))
      (unless (eq group (state-current-group *compositor-state*))
        (state-select-group *compositor-state* (mahogany-group-number group)))
      (let ((frame (or (%group-find-cell group view)
                       (tree:find-view-frame (mahogany-group-tiled-container group)
                                             view)
                       (error "window ~A is not in a frame (hidden?)" id))))
        (state-focus-frame *compositor-state* frame (%cu-seat))
        (%cu-window view)))))

(defun cu-close (id)
  "Ask the window to close, as its close button would."
  (%cu-command
    (hrt:view-request-close (%cu-find-view id))
    (list :closed id)))

(defun cu-select-group (number)
  (%cu-command
    (state-select-group *compositor-state* number)
    (list :group number)))

;;; --- pointer ------------------------------------------------------------

(defparameter *cu-buttons* '((:left . #x110) (:right . #x111) (:middle . #x112)
                             (:side . #x113) (:extra . #x114)))

(defparameter *cu-modifier-keys*
  ;; evdev keycodes of the left-hand modifiers
  '(("ctrl" . 29) ("control" . 29) ("shift" . 42) ("alt" . 56) ("meta" . 56)
    ("super" . 125) ("logo" . 125) ("win" . 125) ("altgr" . 100)))

(defun %cu-button-code (button)
  (or (cdr (assoc button *cu-buttons* :test #'string-equal))
      (error "unknown button ~A (left, right, middle, side, extra)" button)))

(defun %cu-mod-codes (mods)
  "Evdev codes for MODS: NIL, a string such as \"ctrl+shift\", or a list."
  (mapcar (lambda (m)
            (or (cdr (assoc m *cu-modifier-keys* :test #'string-equal))
                (error "unknown modifier ~S" m)))
          (cond ((null mods) nil)
                ((stringp mods)
                 (remove "" (uiop:split-string mods :separator "+")
                         :test #'string=))
                (t mods))))

(defmacro %cu-with-mods ((mods) &body body)
  "Hold the modifiers MODS around BODY."
  (let ((codes (gensym "MODS")))
    `(let ((,codes (%cu-mod-codes ,mods)))
       (dolist (m ,codes) (hrt::hrt-cu-key (%cu-seat) m t))
       (unwind-protect (progn ,@body)
         (dolist (m (reverse ,codes)) (hrt::hrt-cu-key (%cu-seat) m nil))))))

(defun %cu-move (x y)
  (unless (hrt::hrt-cu-pointer-move (%cu-seat) (float x 1d0) (float y 1d0))
    (error "pointer motion failed")))

(defun %cu-pointer ()
  (let ((seat (%cu-seat)))
    (list :pointer (list (round (hrt:hrt-seat-cursor-lx seat))
                         (round (hrt:hrt-seat-cursor-ly seat))))))

(defun cu-move (x y)
  (%cu-command (%cu-move x y) (%cu-pointer)))

(defun cu-button (button state)
  "Press (:down) or release (:up) BUTTON where the pointer is."
  (%cu-command
    (hrt::hrt-cu-pointer-button (%cu-seat) (%cu-button-code button)
                                (and (string-equal state :down) t))
    (%cu-pointer)))

(defun cu-scroll (x y &key (dy 0) (dx 0) pixels mods)
  "Move to X,Y and scroll: DY down (negative up), DX right.  In wheel clicks,
or with PIXELS in pixels as a continuous source (fine, no wheel steps).
MODS are held meanwhile (ctrl+wheel zooms in most programs)."
  (%cu-command
    (%cu-move x y)
    (%cu-with-mods (mods)
      (if pixels
          (progn
            (unless (zerop dy)
              (hrt::hrt-cu-pointer-scroll-pixels (%cu-seat) nil (float dy 1d0)))
            (unless (zerop dx)
              (hrt::hrt-cu-pointer-scroll-pixels (%cu-seat) t (float dx 1d0))))
          (progn
            (unless (zerop dy) (hrt::hrt-cu-pointer-scroll (%cu-seat) nil (round dy)))
            (unless (zerop dx) (hrt::hrt-cu-pointer-scroll (%cu-seat) t (round dx))))))
    (%cu-pointer)))

;;; --- timed sequences ----------------------------------------------------
;;; A drag must unfold over time: a toolkit starts drag-and-drop only after
;;; it has handled the press, and it sees nothing until this thread returns
;;; to the event loop.  So a sequence of actions is played from a timer on
;;; the main thread; the command returns at once with the duration and
;;; cu-busy says when it has finished.  Buttons and keys a sequence holds are
;;; released if it fails, is aborted, or *cu-enabled* goes NIL.

(defvar *cu-player* nil "The timer that plays *cu-queue*.")
(defvar *cu-queue* '() "Actions left: (:move x y) (:down code) (:up code)
(:key-down code) (:key-up code) (:wait ms).")
(defvar *cu-held* '() "(:button . code) and (:key . code) a sequence holds.")

(defun %cu-act (action)
  (let ((seat (%cu-seat)))
    (destructuring-bind (op &optional a b) action
      (ecase op
        (:move (%cu-move a b))
        (:down (hrt::hrt-cu-pointer-button seat a t)
         (push (cons :button a) *cu-held*))
        (:up (hrt::hrt-cu-pointer-button seat a nil)
         (setf *cu-held* (remove (cons :button a) *cu-held* :test #'equal)))
        (:key-down (hrt::hrt-cu-key seat a t)
         (push (cons :key a) *cu-held*))
        (:key-up (hrt::hrt-cu-key seat a nil)
         (setf *cu-held* (remove (cons :key a) *cu-held* :test #'equal)))))))

(defun %cu-release-all ()
  (let ((seat (%cu-seat)))
    (dolist (h *cu-held*)
      (if (eq (car h) :button)
          (hrt::hrt-cu-pointer-button seat (cdr h) nil)
          (hrt::hrt-cu-key seat (cdr h) nil))))
  (setf *cu-held* '() *cu-queue* '()))

(defun %cu-tick (timer)
  (declare (ignore timer))
  (if (not *cu-enabled*)
      (%cu-release-all)
      (handler-case
          (loop while *cu-queue*
                do (let ((action (pop *cu-queue*)))
                     (when (eq (car action) :wait)
                       (hrt:timer-handle-update *cu-player*
                                                (max 1 (round (second action))))
                       (return))
                     (%cu-act action)))
        (error (c)
          (log-string :error "computer use: sequence failed: ~A" c)
          (%cu-release-all)))))

(defun %cu-play (actions)
  "Start playing ACTIONS; their total wait in ms."
  (when *cu-queue*
    (error "a sequence is still playing (cu-busy, cu-abort)"))
  (unless *cu-player*
    (setf *cu-player* (hrt:server-make-timer (state-server *compositor-state*)
                                             #'%cu-tick)))
  (setf *cu-queue* actions)
  (hrt:timer-handle-update *cu-player* 1)
  (loop for a in actions when (eq (car a) :wait) sum (second a)))

(defun cu-busy ()
  "Is a sequence still playing?"
  (%cu-command (list :busy (if *cu-queue* :true :false))))

(defun cu-abort ()
  "Stop a playing sequence and release what it holds."
  (%cu-command (%cu-release-all) (list :aborted :true)))

(defun cu-click (x y &key (button :left) (count 1) mods (press-ms 15)
                         (gap-ms 70))
  "Move to X,Y and click BUTTON COUNT times (2: double click) holding MODS
(\"ctrl\", \"shift+alt\"), as a hand would: each press held PRESS-MS,
GAP-MS between clicks.  Returns at once with duration_ms (cu-busy)."
  (%cu-command
    (let ((code (%cu-button-code button))
          (mod-codes (%cu-mod-codes mods))
          (actions '()))
      (flet ((add (&rest a) (push a actions)))
        (dolist (m mod-codes) (add :key-down m))
        (add :move x y)
        (dotimes (i (max 1 count))
          (when (plusp i) (add :wait gap-ms))
          (add :down code)
          (add :wait press-ms)
          (add :up code))
        (dolist (m (reverse mod-codes)) (add :key-up m)))
      (list :pointer (list x y) :clicks (max 1 count)
            :duration-ms (%cu-play (nreverse actions))))))

(defun cu-drag (points &key (button :left) (steps 10) (step-ms 10)
                         (pause-ms 0) (hold-ms 50) mods)
  "Drag with BUTTON (holding MODS) along POINTS, a flat list x1 y1 x2 y2 ...:
press at the first point, glide to each next one in STEPS motions STEP-MS
apart, stop PAUSE-MS at every waypoint, release at the last.  HOLD-MS before
and after the press and before the release lets the client react.  Returns
at once; the drag takes duration_ms (cu-busy)."
  (%cu-command
    (unless (and (>= (length points) 4) (evenp (length points)))
      (error "a drag needs two or more x y points"))
    (let* ((code (%cu-button-code button))
           (mod-codes (%cu-mod-codes mods))
           (pts (loop for (x y) on points by #'cddr collect (cons x y)))
           (steps (max 1 steps))
           (actions '()))
      (flet ((add (&rest a) (push a actions)))
        (dolist (m mod-codes) (add :key-down m))
        (add :move (car (first pts)) (cdr (first pts)))
        (add :wait hold-ms)
        (add :down code)
        (add :wait hold-ms)
        (loop for (from to) on pts while to
              do (loop for i from 1 to steps
                       do (add :move
                               (+ (car from) (* (- (car to) (car from)) (/ i steps)))
                               (+ (cdr from) (* (- (cdr to) (cdr from)) (/ i steps))))
                          (add :wait step-ms))
                 (when (plusp pause-ms) (add :wait pause-ms)))
        (add :wait hold-ms)
        (add :up code)
        (dolist (m (reverse mod-codes)) (add :key-up m)))
      (list :points (length pts)
            :duration-ms (%cu-play (nreverse actions))))))

;;; --- keyboard -----------------------------------------------------------

(defparameter *cu-key-aliases*
  '(("enter" . "Return") ("ret" . "Return") ("esc" . "Escape")
    ("del" . "Delete") ("bs" . "BackSpace") ("backspace" . "BackSpace")
    ("pgup" . "Prior") ("pageup" . "Prior") ("pgdn" . "Next")
    ("pagedown" . "Next") ("space" . "space") ("tab" . "Tab")
    ("up" . "Up") ("down" . "Down") ("left" . "Left") ("right" . "Right")
    ("print" . "Print") ("prtsc" . "Print")))

(defun %cu-keysym-key (keysym)
  "The evdev keycode for KEYSYM in the layout in effect and the evdev codes
of the modifiers it needs (Shift, AltGr), or NIL."
  (cffi:with-foreign-objects ((key :uint32) (mods :uint32 4))
    (let ((n (hrt::hrt-cu-keysym-lookup (%cu-seat) keysym key mods 4)))
      (if (minusp n)
          (values nil nil)
          (values (cffi:mem-ref key :uint32)
                  (loop for i below n collect (cffi:mem-aref mods :uint32 i)))))))

(defun %cu-named-key (name)
  "The evdev keycode and needed modifiers for a key NAME: an xkb keysym name
(Return, F5, XF86AudioMute, a, A, plus) or an alias (enter, esc, pgup)."
  (let* ((alias (cdr (assoc name *cu-key-aliases* :test #'string-equal)))
         (sym (hrt::hrt-cu-keysym-from-name (or alias name))))
    (when (zerop sym)
      (error "unknown key name ~S" name))
    (multiple-value-bind (key mods) (%cu-keysym-key sym)
      (unless key
        (error "key ~S is not on the keyboard map" name))
      (values key mods))))

(defun %cu-tap (key mods)
  "Press MODS (evdev codes) in order, tap KEY, release MODS in reverse."
  (let ((seat (%cu-seat)))
    (dolist (m mods) (hrt::hrt-cu-key seat m t))
    (hrt::hrt-cu-key seat key t)
    (hrt::hrt-cu-key seat key nil)
    (dolist (m (reverse mods)) (hrt::hrt-cu-key seat m nil))))

(defun %cu-chord (spec)
  "Tap one chord such as \"ctrl+shift+t\", \"super+Return\", \"F5\"."
  (let* ((parts (uiop:split-string spec :separator "+"))
         ;; "ctrl++" is ctrl and plus
         (parts (if (and (>= (length parts) 2)
                         (string= (car (last parts)) "")
                         (string= (car (last parts 2)) ""))
                    (append (butlast parts 2) (list "plus"))
                    parts))
         (key-name (car (last parts)))
         (mods (mapcar (lambda (m)
                         (or (cdr (assoc m *cu-modifier-keys* :test #'string-equal))
                             (error "unknown modifier ~S in ~S" m spec)))
                       (butlast parts))))
    (multiple-value-bind (key needed) (%cu-named-key key-name)
      (%cu-tap key (append mods (set-difference needed mods))))))

(defun cu-key (&rest chords)
  "Tap each chord in turn: (cu-key \"ctrl+l\") (cu-key \"super+2\" \"Return\")."
  (%cu-command
    (dolist (c chords) (%cu-chord c))
    (list :keys chords)))

(defun cu-key-hold (name state)
  "Press (:down) or release (:up) one key or modifier by name, for chords
the other commands cannot express (hold shift across clicks)."
  (%cu-command
    (let ((code (or (cdr (assoc name *cu-modifier-keys* :test #'string-equal))
                    (%cu-named-key name))))
      (hrt::hrt-cu-key (%cu-seat) code (and (string-equal state :down) t))
      (list :key name :state (string-downcase (string state))))))

(defun cu-type (text)
  "Type TEXT with the layout in effect, holding whatever the character's
level needs (Shift, AltGr: @ is AltGr+v on cz), newline as Return, tab as Tab.  Characters the map cannot produce are skipped and
reported."
  (%cu-command
    (let ((skipped '()) (typed 0))
      (loop for c across text
            do (let ((sym (case c
                            (#\Newline #xff0d)          ; Return
                            (#\Tab #xff09)              ; Tab
                            (t (hrt::hrt-cu-char-keysym (char-code c))))))
                 (multiple-value-bind (key mods) (and (plusp sym) (%cu-keysym-key sym))
                   (if key
                       (progn (%cu-tap key mods) (incf typed))
                       (push (string c) skipped)))))
      (list :typed typed :skipped (or (nreverse skipped) #())))))

;;; --- screenshots and programs -------------------------------------------

(defun cu-screenshot (path &key (scale 1) x y width height)
  "Capture the screen (or the region X,Y WIDTHxHEIGHT, layout pixels) to the
PNG PATH, scaled by SCALE.  grim runs in the background (it needs this
thread to serve its screencopy): PATH appears, complete, when it is done;
it writes PATH.part first."
  (%cu-command
    (when *idle-blanked*
      (error "the panel is blanked for idleness, a shot would be black: wake it first (cu-wake)"))
    (let ((args (append (list "/usr/pkg/bin/grim" "-s" (format nil "~,3F" scale))
                        (when (and x y width height)
                          (list "-g" (format nil "~D,~D ~Dx~D" x y width height)))
                        (list "-t" "png"))))
      (uiop:launch-program
       (list "/bin/sh" "-c"
             (format nil "~{'~A'~^ ~} \"$1.part\" && mv \"$1.part\" \"$1\""
                     args)
             "cu-screenshot" path)
       :output nil :error-output nil)
      (list :path path :scale scale))))

(defun cu-wake ()
  "Wake a panel the idle timer dimmed or blanked, the way any input does: a
pointer motion to where the pointer already is.  The panel comes back on
over the next frames; cu-screen reports panel \"on\" then."
  (%cu-command
    (let ((seat (%cu-seat)))
      (%cu-move (hrt:hrt-seat-cursor-lx seat) (hrt:hrt-seat-cursor-ly seat))
      (list :panel (cond (*idle-blanked* "blanked")
                         (*idle-dimmed* "dimmed")
                         (t "on"))))))

(defun cu-launch (command)
  "Start COMMAND (a shell command line) in the session, detached."
  (%cu-command
    (let ((p (uiop:launch-program (list "/bin/sh" "-c" command)
                                  :output nil :error-output nil)))
      (list :pid (or (ignore-errors (uiop:process-info-pid p)) :null)))))

;;; The control protocol is one form per line: clients send free text (to
;;; type, or a command line) hex-encoded UTF-8 so that newlines and quotes
;;; need no escaping.

(defun %cu-unhex (hex)
  (sb-ext:octets-to-string
   (coerce (loop for i from 0 below (length hex) by 2
                 collect (parse-integer hex :start i :end (+ i 2) :radix 16))
           '(vector (unsigned-byte 8)))
   :external-format :utf-8))

(defun cu-type-hex (hex) (cu-type (%cu-unhex hex)))

(defun cu-launch-hex (hex) (cu-launch (%cu-unhex hex)))

(defun cu-find-app (app-id)
  "The mapped windows whose app_id is APP-ID or starts with it: \"firefox\"
finds Nightly's \"firefox-default\"."
  (%cu-command
    (or (loop for v in (%cu-views)
              for id = (or (hrt::hrt-view-app-id (hrt:view-hrt-view v)) "")
              when (and (<= (length app-id) (length id))
                        (string= app-id id :end2 (length app-id)))
                collect (%cu-window v))
        #())))
