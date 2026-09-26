;;;; A control socket for scripts and remote work: each connection sends one
;;;; Lisp form on a line, the compositor evaluates it on its main thread in
;;;; the MAHOGANY package, and the printed result comes back on one line.
;;;; The socket lives in XDG_RUNTIME_DIR, which is mode 0700, so only its
;;;; owner and root can reach it. /usr/local/bin/mahoganyctl is the client.
(in-package #:mahogany)

(defvar *control-socket* nil)
(defvar *control-thread* nil)
(defvar *control-timeout* 10 "Seconds to wait for the main thread.")

(defun control-socket-path ()
  (alexandria:when-let ((dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (concatenate 'string dir "/mahogany.sock")))

(defun %control-eval (line)
  "Evaluate LINE on the main thread and return the printed result."
  (let ((done (sb-thread:make-semaphore))
        (result "ERROR: not run"))
    (hrt:run-in-main-thread
     (lambda ()
       (setf result
             (handler-case
                 (let ((*package* (find-package '#:mahogany))
                       (*read-eval* nil))
                   (let ((*print-length* 50) (*print-level* 5))
                     (format nil "~S" (eval (read-from-string line)))))
               (error (c) (format nil "ERROR: ~A" c))))
       (sb-thread:signal-semaphore done)))
    (if (sb-thread:wait-on-semaphore done :timeout *control-timeout*)
        (substitute #\Space #\Newline result)
        "ERROR: timed out waiting for the main thread")))

(defun %control-serve (server)
  (loop
    (let ((client (handler-case (sb-bsd-sockets:socket-accept server)
                    (error () (return)))))
      (handler-case
          (let ((stream (sb-bsd-sockets:socket-make-stream
                         client :input t :output t :buffering :full
                                :element-type 'character :external-format :utf-8)))
            (unwind-protect
                 (alexandria:when-let ((line (read-line stream nil nil)))
                   (write-line (%control-eval line) stream)
                   (finish-output stream))
              (close stream)))
        (error (c)
          (log-string :warn "control socket: ~A" c)
          (ignore-errors (sb-bsd-sockets:socket-close client)))))))

(defun control-start ()
  "Open the control socket. Safe to call again."
  (let ((path (control-socket-path)))
    (when (and path (not *control-socket*))
      (ignore-errors (delete-file path))
      (handler-case
          (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
            (sb-bsd-sockets:socket-bind socket path)
            (sb-bsd-sockets:socket-listen socket 4)
            (setf *control-socket* socket
                  *control-thread* (bt2:make-thread (lambda () (%control-serve socket))
                                                    :name "control socket"))
            (log-string :info "Control socket at ~A" path))
        (error (c) (log-string :error "control socket not opened: ~A" c))))))

(defun control-stop ()
  (when *control-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *control-socket*))
    (setf *control-socket* nil)
    (alexandria:when-let ((path (control-socket-path)))
      (ignore-errors (delete-file path)))))
