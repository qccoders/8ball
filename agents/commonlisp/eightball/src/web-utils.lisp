(in-package #:eightball)

(defmacro with-request-data 
  ((&key (req (gensym)) 
         (parms nil)
         (headers nil)
         (path nil)
         (method nil)
         )
         &body body)
    `(let* ((,req hunchentoot:*request*)
            ,@(when path 
                `((,path (hunchentoot:request-uri ,req))))
            ,@(when method 
                `((,method (hunchentoot:request-method* ,req))))
            ,@(when parms 
                 (mapcar (lambda (p) 
                  `(,(first p)
                     (hunchentoot:parameter ,(second p) ,req))) 
                         parms))
            ,@(when headers 
                 (mapcar (lambda (p) 
                  `(,(first p)
                     (hunchentoot:header-in ,(second p) ,req)))
                          headers)))
      ,@body))


(defmacro handler (name path &body body)
	`(progn
 		(defun ,name ()
			,@body)
   		(setf (gethash ,path *handlers*)
           (hunchentoot:create-regex-dispatcher 
             ,path 
             (find-symbol ,(string name))))
     	(when *init-handlers* (init-handlers))))


(defun is-method? (r m)
  `(equal ,m (hunchentoot:request-method* ,r)))


(defmacro is-put? (r)
  (is-method? r :PUT))

(defmacro is-delete? (r)
  (is-method? r :DELETE))


(defun get-header (r s)
  (hunchentoot:header-in s r))


(defun json->response (j)
	(setf (hunchentoot:content-type*) "application/json")
 	(setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
 	(setf (hunchentoot:header-out "Access-Control-Allow-Headers") "content-type")
    (o->json-string j))
