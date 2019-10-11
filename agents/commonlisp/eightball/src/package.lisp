(defpackage #:eightball
  (:use #:cl 
        #:cl-arrows
        #:defclass-std)
   (:export
     #:start-server
     #:start-server-threaded
     #:stop-server))
