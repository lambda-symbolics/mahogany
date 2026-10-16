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

(defvar *control-quiet-forms* '("(bar-status-changed)")
  "Requests sent routinely (the status sampler): not logged.")

(defun %control-peer-pid (client)
  "The pid of the process at the other end of CLIENT (NetBSD LOCAL_PEEREID:
struct unpcbid {pid_t pid; uid_t euid; gid_t egid;}), or NIL."
  #+netbsd
  (ignore-errors
   (sb-alien:with-alien ((id (array (sb-alien:signed 32) 3))
                         (len sb-alien:unsigned-int))
     (setf len 12)
     (when (zerop (sb-alien:alien-funcall
                   (sb-alien:extern-alien
                    "getsockopt"
                    (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int
                              (* t) (* sb-alien:unsigned-int)))
                   (sb-bsd-sockets:socket-file-descriptor client)
                   0 3                  ; SOL_LOCAL, LOCAL_PEEREID
                   (sb-alien:alien-sap id) (sb-alien:addr len)))
       (sb-alien:deref id 0))))
  #-netbsd nil)

(defun %control-describe-peer (pid)
  "\"pid COMMAND <- ppid PARENT\" for the log, while the client still runs."
  (flet ((ps (field p)
           (string-trim '(#\Space #\Tab #\Newline)
                        (or (ignore-errors
                             (uiop:run-program (list "/bin/ps" "-ww" "-o" field
                                                     "-p" (princ-to-string p))
                                               :output :string
                                               :ignore-error-status t))
                            ""))))
    (if (null pid)
        "unknown peer"
        (let ((ppid (parse-integer (ps "ppid=" pid) :junk-allowed t)))
          (format nil "~D ~A <- ~A ~A" pid (ps "command=" pid)
                  (or ppid "?") (if ppid (ps "command=" ppid) ""))))))

(defun %control-eval (line &optional (peer "unknown peer"))
  "Evaluate LINE on the main thread and return the printed result.  Every
request but the routine ones is logged with its caller: a stray (lid-closed)
once kept the panel off and nothing showed who had sent it."
  (unless (member (string-trim " " line) *control-quiet-forms* :test #'string=)
    (log-string :info "control: ~A from ~A"
                (if (> (length line) 200) (subseq line 0 200) line) peer))
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
                   (write-line
                    (%control-eval
                     line
                     (if (member (string-trim " " line) *control-quiet-forms*
                                 :test #'string=)
                         "statusbard"
                         (%control-describe-peer (%control-peer-pid client))))
                    stream)
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
