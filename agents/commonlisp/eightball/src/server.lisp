(in-package #:eightball)

(defvar *server* nil)


(defun make-handlers ()
  (alexandria:hash-table-values *handlers*))


(defun init-handlers ()
  (setq hunchentoot:*dispatch-table*
      (append (make-handlers)))
  (setq *init-handlers* t))


(defun start-server (f)
    (load-config f)
    (let ((port (config-int "port" "8080")))
        (format t "initializing server on port: ~D ~%" port)
        (init-handlers)
        (setq *server* 
          (make-instance 
            'hunchentoot:easy-acceptor 
            :port port))
        (hunchentoot:start *server*)

        (when (and (config "mothership") (config "find-me-at"))
            (register-with-mothership 
              (config "mothership") 
              (config "find-me-at")))
        (format t "server started~%" port)))



(defun stop-server ()
	(format t "stopping server~%")
	(hunchentoot:stop *server*)
  (setq *server* nil)
  (format t "server shut down successfully!~%"))


(defun config-opts ()
    (opts:define-opts
        (:name :conf-file
         :description "path to the configuration file"
         :short #\f
         :long "conf-file"
         :arg-parser #'string)))


(defun start-server-threaded ()
    (config-opts)
    (multiple-value-bind (options free-args) (opts:get-opts)

    (format t "conf file at: ~A~%" 
            (getf options :conf-file))
                         
      (start-server 
        (if (getf options :conf-file) 
            (getf options :conf-file)
            "server.conf"))
      (handler-case
         (sb-thread:join-thread 
             (find-if (lambda (th) 
              (search "hunchentoot-listener" (sb-thread:thread-name th)))
             (sb-thread:list-all-threads)))
         (sb-sys:interactive-interrupt () 
             (progn
                (stop-server)
                (uiop:quit)))
         (error (c) 
             (format t "Woops, an unknown error occured!:~&~a~&" c)))))
