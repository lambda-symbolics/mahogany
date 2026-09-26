;;;; The status bar: one cairo-rendered line per output, drawn by heart on
;;;; the top layer. The left side shows the group and its windows, the right
;;;; side whatever the status sampler publishes. Nothing repaints unless the
;;;; text changes.
(in-package #:mahogany)

(defvar *bar-enabled* t)
(defvar *bar-bottom* nil "Place the bar at the bottom of the output.")
(defvar *bar-font* "monospace 9")
(defvar *bar-foreground* "#ffffff")
(defvar *bar-background* "#000000")
(defvar *bar-pad-x* 2)
(defvar *bar-pad-y* 1)
(defvar *bar-status-file* "/tmp/.statusline"
  "Whitespace separated key=value snapshot written by the status sampler.")
(defvar *bar-status-interval* 3 "Seconds between reads of the status file.")
(defvar *bar-title-width* 20)

(defvar *bar-right-function* 'bar-default-right
  "Function of the status line returning the right-hand pango markup.")
(defvar *bar-left-function* 'bar-default-left
  "Function of the group returning the left-hand pango markup.")

(defvar *bar-status* "")
(defvar *bar-thread* nil)
(defvar *bar-last* (make-hash-table :test 'equal)
  "Output name -> (left . right) last drawn, to skip unchanged repaints.")
(defvar *bar-refreshing* nil)

(defun bar-theme ()
  (mh/theme:make-theme :font *bar-font*
                       :font-color (colors:as-rgb *bar-foreground*)
                       :background-color (colors:as-rgb *bar-background*)
                       :border-color (colors:as-rgb *bar-background*)))

(defun bar-escape (text)
  "Escape TEXT for pango markup."
  (with-output-to-string (s)
    (loop for c across text
          do (case c
               (#\& (write-string "&amp;" s))
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (t (write-char c s))))))

(defun bar-block (bg fg text)
  "A coloured block with a little horizontal padding, waybar style."
  (format nil "<span background=\"~a\" foreground=\"~a\"> ~a </span>"
          bg fg (bar-escape text)))

(defun bar-get (line key &optional (default "?"))
  "Value of KEY=... in the whitespace-separated status LINE."
  (let* ((needle (concatenate 'string key "="))
         (pos (search needle line)))
    (if pos
        (let* ((start (+ pos (length needle)))
               (end (or (position #\Space line :start start) (length line)))
               (v (subseq line start end)))
          (if (zerop (length v)) default v))
        default)))

(defun bar-battery-block (line)
  (let* ((p (bar-get line "bat")) (st (bar-get line "bats"))
         (pn (or (parse-integer p :junk-allowed t) 100))
         (prefix (cond ((string= st "chg") "[++]") ((string= st "plug") "[==]")
                       ((string= st "full") "[00]") (t "[--]")))
         (text (format nil "~a~a% BAT" prefix p)))
    (cond ((member st '("chg" "plug" "full") :test #'string=)
           (bar-block "#26A65B" "#ffffff" text))
          ((<= pn 15) (bar-block "#f53c3c" "#ffffff" text))
          (t (bar-block "#ffffff" "#000000" text)))))

(defun bar-audio-block (line)
  (let ((vol (if (string= (bar-get line "volm" "off") "on") "MUTED"
                 (format nil "~a% VOL" (bar-get line "vol"))))
        (mic (if (string= (bar-get line "micm" "off") "on") "MUTED"
                 (format nil "~a% MIC" (bar-get line "mic")))))
    (bar-block "#8ba4b0" "#000000" (format nil "~a ~a" vol mic))))

(defun bar-default-right (line)
  "Right side, waybar order: RAM TEMP CPU AUDIO [WIFI] BAT CLOCK."
  (handler-case
      (let ((ssid (bar-get line "ssid" "")))
        (format nil "~a ~a ~a ~a ~a~a ~a"
                (bar-block "#000000" "#ffffff" (format nil "~a% RAM" (bar-get line "ram")))
                (bar-block "#000000" "#ffffff" (format nil "~a~aC" (bar-get line "temp") (code-char 176)))
                (bar-block "#E6C384" "#000000" (format nil "~a% CPU" (bar-get line "cpu")))
                (bar-audio-block line)
                (if (plusp (length ssid))
                    (concatenate 'string (bar-block "#2980b9" "#ffffff" ssid) " ") "")
                (bar-battery-block line)
                (bar-block "#E46876" "#000000" (bar-get line "clk" "--:--"))))
    (error () "")))

(defun bar-window-entry (index cell current)
  (let* ((view (tree:frame-surface cell))
         (title (or (and view (hrt:view-title view)) ""))
         (title (if (> (length title) *bar-title-width*)
                    (subseq title 0 *bar-title-width*)
                    title)))
    (format nil "~a~d ~a"
            (cond ((eq cell current) "*") (t " "))
            index
            (bar-escape title))))

(defun bar-default-left (group)
  "StumpWM's \"[group] windows\": the group name in bold, then every window
of the current strip in column order, the focused one starred."
  (let* ((current (mahogany-group-current-frame group))
         (strip (group-strip group))
         (cells (and strip (tree:strip-cells strip))))
    (format nil "<b>[~a]</b> ~{~a~^  ~}"
            (bar-escape (mahogany-group-name group))
            (loop for cell in cells
                  for i from 0
                  collect (bar-window-entry i cell current)))))

(defun %bar-apply-height (output height)
  "Reserve HEIGHT pixels for the bar in every strip on OUTPUT."
  (let ((name (hrt:output-full-name output)))
    (loop for group across (state-groups *compositor-state*)
          for node = (gethash name (mahogany-group-output-map group))
          for strip = (and node (output-node-strip node))
          when strip
            do (if *bar-bottom*
                   (setf (tree:strip-reserved-bottom strip) height)
                   (setf (tree:strip-reserved-top strip) height))
               (tree:strip-layout strip))))

(defun bar-refresh (&key force)
  "Redraw the bar on every output whose text changed."
  (unless (or *bar-refreshing* (not (state-server *compositor-state*)))
    (let ((*bar-refreshing* t))
      (loop for container across (state-cur-outputs *compositor-state*)
            for output = (tree:output-container-output container)
            for name = (hrt:output-full-name output)
            do (if *bar-enabled*
                   (let* ((group (state-current-group *compositor-state*))
                          (left (funcall *bar-left-function* group))
                          (right (funcall *bar-right-function* *bar-status*))
                          (last (gethash name *bar-last*)))
                     (unless (and (not force) last
                                  (string= (car last) left) (string= (cdr last) right))
                       (let ((height (hrt:bar-set output left right (bar-theme)
                                                  :bottom *bar-bottom*
                                                  :pad-x *bar-pad-x* :pad-y *bar-pad-y*)))
                         (log-string :debug "Bar on ~A: height ~A, ~D+~D chars"
                                     name height (length left) (length right))
                         (setf (gethash name *bar-last*) (cons left right))
                         (unless (eql (gethash (list name :height) *bar-last*) height)
                           (setf (gethash (list name :height) *bar-last*) height)
                           (%bar-apply-height output height)))))
                   (when (gethash name *bar-last*)
                     (hrt:bar-clear output)
                     (remhash name *bar-last*)
                     (remhash (list name :height) *bar-last*)
                     (%bar-apply-height output 0)))))))

(defun bar-read-status-file ()
  (handler-case
      (with-open-file (s *bar-status-file* :if-does-not-exist nil)
        (if s (or (read-line s nil nil) *bar-status*) *bar-status*))
    (error () *bar-status*)))

(defun %bar-status-loop ()
  (loop
    ;; While the panel is off nobody sees the bar and the sampler is paused;
    ;; wake far less often.
    (sleep (if *idle-blanked* 30 *bar-status-interval*))
    ;; bt2:make-thread returns a wrapper, so compare with bt2:current-thread;
    ;; a restarted reader replaces *bar-thread* and this one bows out.
    (unless (eq (bt2:current-thread) *bar-thread*)
      (return))
    (let ((line (if *idle-blanked* *bar-status* (bar-read-status-file))))
      (unless (string= line *bar-status*)
        (hrt:with-main-thread ()
          (setf *bar-status* line)
          (bar-refresh))))))

(defun bar-start ()
  "Draw the bar and start the status reader thread. Safe to call again."
  (setf *bar-status* (bar-read-status-file))
  (bar-refresh :force t)
  (unless (and *bar-thread* (bt2:thread-alive-p *bar-thread*))
    (setf *bar-thread*
          (bt2:make-thread #'%bar-status-loop :name "status bar reader"))))

(defun bar-stop ()
  (setf *bar-thread* nil)
  (let ((*bar-enabled* nil))
    (bar-refresh)))

(defvar *bar-refresh-timer* nil)

(defun bar-schedule-refresh ()
  "Redraw shortly, once the current operation has settled. Layout runs in
the middle of focus changes, when the group's current frame is transiently
unset, and several layouts in a row become one repaint."
  (when (state-server *compositor-state*)
    (unless *bar-refresh-timer*
      (setf *bar-refresh-timer*
            (hrt:server-make-timer (state-server *compositor-state*)
                                   (lambda (timer)
                                     (declare (ignore timer))
                                     (bar-refresh)))))
    (hrt:timer-handle-update *bar-refresh-timer* 30)))

(defun %bar-layout-hook (strip)
  (declare (ignore strip))
  (bar-schedule-refresh))

(pushnew '%bar-layout-hook tree:*strip-layout-hook*)

(defcommand bar-toggle ()
  (:documentation "Show or hide the status bar")
  (:method ()
    (setf *bar-enabled* (not *bar-enabled*))
    (bar-refresh :force t)))
