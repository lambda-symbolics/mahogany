;;;; Hand-written bindings for heart/src/lispbsd.c. The generated bindings in
;;;; hrt-bindings.lisp are left untouched so cl-bindgen can still regenerate
;;;; them from the upstream headers.
(in-package #:hrt)

(cffi:defcfun ("hrt_view_pid" hrt-view-pid) :int
  (view (:pointer (:struct hrt-view))))

(cffi:defcfun ("hrt_bar_set" hrt-bar-set) :int
  (output (:pointer (:struct hrt-output)))
  (left :string)
  (right :string)
  (theme (:pointer (:struct hrt-message-theme)))
  (bottom :bool))

(cffi:defcfun ("hrt_bar_clear" hrt-bar-clear) :void
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_set_power" hrt-output-set-power) :bool
  (output (:pointer (:struct hrt-output)))
  (on :bool))

(cffi:defcfun ("hrt_idle_inhibitor_count" hrt-idle-inhibitor-count) :int)

(cffi:defcfun ("hrt_set_pointer_enter_callback" hrt-set-pointer-enter-callback) :void
  (callback :pointer))

(cffi:defcfun ("hrt_set_activity_callback" hrt-set-activity-callback) :void
  (callback :pointer))

(cffi:defcfun ("hrt_arm_activity_callback" hrt-arm-activity-callback) :void)

(cffi:defcfun ("hrt_ms_since_activity" hrt-ms-since-activity) :int64)

(cffi:defcfun ("hrt_view_set_suspended" hrt-view-set-suspended) :void
  (view (:pointer (:struct hrt-view)))
  (suspended :bool))

(defun view-set-suspended (view suspended)
  "Tell VIEW's client whether it is visible; only changes are sent."
  (declare (type view view))
  (let ((suspended (and suspended t)))
    (unless (eq (view-suspended view) suspended)
      (setf (view-suspended view) suspended)
      (hrt-view-set-suspended (view-hrt-view view) suspended))))

(cffi:defcfun ("hrt_output_frames_rendered" hrt-output-frames-rendered) :uint64
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_refresh" hrt-output-refresh) :int
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_set_refresh" hrt-output-set-refresh) :int
  (output (:pointer (:struct hrt-output)))
  (refresh-mhz :int))

(defun output-frames-rendered (output)
  "Frames actually rendered on OUTPUT so far."
  (declare (type output output))
  (hrt-output-frames-rendered (output-hrt-output output)))

(defun output-refresh (output)
  "Refresh rate of OUTPUT's current mode in mHz, 0 when unknown."
  (declare (type output output))
  (hrt-output-refresh (output-hrt-output output)))

(defun output-set-refresh (output refresh-mhz)
  "Switch OUTPUT to the same-resolution mode closest to REFRESH-MHZ.
Returns the refresh chosen, 0 on failure."
  (declare (type output output))
  (hrt-output-set-refresh (output-hrt-output output) refresh-mhz))

(defun view-pid (view)
  "The pid of the client owning VIEW, or NIL."
  (declare (type view view))
  (let ((pid (hrt-view-pid (view-hrt-view view))))
    (and (plusp pid) pid)))

(defun bar-set (output left right theme &key (bottom nil) (pad-x 2) (pad-y 1))
  "Draw the status bar on OUTPUT. LEFT and RIGHT are pango markup strings.
Returns the bar height in layout pixels, 0 on failure."
  (declare (type output output)
           (type string left right)
           (type mh/theme:theme theme))
  (cffi:with-foreign-strings ((l left) (r right) (c-font (mh/theme:theme-font theme)))
    (with-foreign-struct-init (message-theme (:struct hrt-message-theme))
        ((font c-font)
         (message-padding pad-x)
         (message-border-width pad-y)
         (margin-x 0)
         (margin-y 0))
      (write-color-array
       (cffi:foreign-slot-pointer message-theme '(:struct hrt-message-theme) 'font-color)
       (mh/theme:theme-font-color theme))
      (write-color-array
       (cffi:foreign-slot-pointer message-theme '(:struct hrt-message-theme) 'background-color)
       (mh/theme:theme-background-color theme))
      (write-color-array
       (cffi:foreign-slot-pointer message-theme '(:struct hrt-message-theme) 'border-color)
       (mh/theme:theme-border-color theme))
      (hrt-bar-set (output-hrt-output output) l r message-theme bottom))))

(defun bar-clear (output)
  (declare (type output output))
  (hrt-bar-clear (output-hrt-output output)))

(defun output-set-power (output on)
  "Turn the panel of OUTPUT on or off without removing it from the layout."
  (declare (type output output))
  (hrt-output-set-power (output-hrt-output output) (and on t)))

(defun idle-inhibited-p ()
  "True while any client holds an idle inhibitor (video playback and the like)."
  (plusp (hrt-idle-inhibitor-count)))

;;; Computer use (heart/src/computer_use.c, lisp/computer-use.lisp)

(cffi:defcfun ("hrt_cu_pointer_move" hrt-cu-pointer-move) :bool
  (seat :pointer) (lx :double) (ly :double))

(cffi:defcfun ("hrt_cu_pointer_button" hrt-cu-pointer-button) :bool
  (seat :pointer) (button :uint32) (pressed :bool))

(cffi:defcfun ("hrt_cu_pointer_scroll" hrt-cu-pointer-scroll) :bool
  (seat :pointer) (horizontal :bool) (clicks :int))

(cffi:defcfun ("hrt_cu_pointer_scroll_pixels" hrt-cu-pointer-scroll-pixels) :bool
  (seat :pointer) (horizontal :bool) (pixels :double))

(cffi:defcfun ("hrt_cu_key" hrt-cu-key) :bool
  (seat :pointer) (key :uint32) (pressed :bool))

(cffi:defcfun ("hrt_cu_keysym_lookup" hrt-cu-keysym-lookup) :int
  (seat :pointer) (keysym :uint32) (key :pointer) (mod-keys :pointer)
  (max-mods :int))

(cffi:defcfun ("hrt_cu_char_keysym" hrt-cu-char-keysym) :uint32
  (codepoint :uint32))

(cffi:defcfun ("hrt_cu_keysym_from_name" hrt-cu-keysym-from-name) :uint32
  (name :string))

(cffi:defcfun ("hrt_view_geometry" hrt-view-geometry) :bool
  (view :pointer) (x :pointer) (y :pointer) (width :pointer) (height :pointer))

(cffi:defcfun ("hrt_view_app_id" hrt-view-app-id) :string
  (view :pointer))

(cffi:defcfun ("hrt_layout_box" hrt-layout-box) :void
  (server :pointer) (x :pointer) (y :pointer) (width :pointer) (height :pointer))

;;; Output mirroring (heart/src/mirror.c, lisp/mirror.lisp)

(cffi:defcfun ("hrt_mirror_start" hrt-mirror-start) :pointer
  (source :pointer) (target :pointer))

(cffi:defcfun ("hrt_mirror_set_view" hrt-mirror-set-view) :void
  (mirror :pointer)
  (src-x :double) (src-y :double) (src-width :double) (src-height :double)
  (x :int) (y :int) (width :int) (height :int)
  (target-width :int) (target-height :int) (smooth :bool))

(cffi:defcfun ("hrt_mirror_set_frozen" hrt-mirror-set-frozen) :void
  (mirror :pointer) (frozen :bool))

(cffi:defcfun ("hrt_mirror_alive" hrt-mirror-alive) :bool
  (mirror :pointer))

(cffi:defcfun ("hrt_mirror_frames" hrt-mirror-frames) :uint64
  (mirror :pointer))

(cffi:defcfun ("hrt_mirror_stop" hrt-mirror-stop) :void
  (mirror :pointer) (restore :bool))

(cffi:defcfun ("hrt_mirror_capture" hrt-mirror-capture) :bool
  (mirror :pointer) (path :string))

(cffi:defcfun ("hrt_outputs_rescan" hrt-outputs-rescan) :int
  (server :pointer))

(cffi:defcfun ("hrt_output_pixel_size" hrt-output-pixel-size) :void
  (output :pointer) (width :pointer) (height :pointer))

(defun output-pixel-size (output)
  "OUTPUT's mode in pixels: the size of the buffers it shows."
  (declare (type output output))
  (with-return-by-value ((width :int) (height :int))
    (hrt-output-pixel-size (output-hrt-output output) width height)))

(cffi:defcfun ("hrt_seat_warp_cursor" hrt-seat-warp-cursor) :void
  (seat :pointer) (lx :double) (ly :double))
