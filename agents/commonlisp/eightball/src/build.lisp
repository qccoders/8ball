(ql:quickload "eightball")


(defun make-exe ()
	(sb-ext:save-lisp-and-die 
	  "./dist/eightball"
	  :toplevel #'eightball:start-server-threaded
	  :executable t))


(make-exe)

