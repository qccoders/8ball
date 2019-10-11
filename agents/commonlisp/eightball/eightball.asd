(defpackage :eightball (:use :cl :asdf))
(in-package :eightball)


(asdf:defsystem #:eightball
  :name "eightball"
  :serial t
  :depends-on (#:hunchentoot 
               #:cl-json 
               #:cl-arrows 
               #:unix-opts
               #:uuid
               #:drakma
               #:defclass-std 
               #:cl-strings 
               #:exit-hooks
               #:alexandria)
  :description "handles 8-ball questions"
  :author "jeff"
  :license "whatever"
  :components
  ((:module "src"
            :serial t
            :components ((:file "package")
                         (:file "globals" :depends-on ("package"))
                         (:file "config" :depends-on ("globals"))
                         (:file "web-utils" :depends-on ("config"))
                         (:file "strategies" :depends-on ("web-utils"))
                         (:file "handlers" :depends-on ("strategies"))
                         (:file "mothership" :depends-on ("web-utils"))
                         (:file "server" :depends-on ("mothership"))))))


