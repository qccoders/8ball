(in-package #:eightball)

(handler status "^/$"
  (json->response "running!"))



(defvar *sub-agents* (make-hash-table :test #'equal))

(defun add-sub-agent (k v)
   (format t "adding sub-agent: ~S~%" v)
   (setf (gethash k *sub-agents*) v))

(defun remove-sub-agent (k)
   (format t "removing sub-agent: ~S~%" k)
   (remhash k *sub-agents*))

(defun print-sub-agent (key value)
   (format t "bearer : ~S, url: ~S~%" key value))

(defun print-sub-agents ()
   (format t "sub-agents:~%")
   (maphash #'print-sub-agent *sub-agents*))

(defun has-sub-agents? ()
  (> (hash-table-count *sub-agents*) 0))

(handler agents "^/webhooks/.*$" 
   (with-request-data (:method method :path path :headers ((auth "Authorization")))
       (case method 
          (:PUT (add-sub-agent auth (string-after-prefix path "/webhooks/")))
          (:DELETE (remove-sub-agent auth))
          (T (print-sub-agents)))
       (json->response "ok")))





(defclass/std answer-response ()
  ((name response delay children :i)))


(defun fetch-from-sub-agent (question ttl url)
  (format t "fetching from subagent: ~a, q:~a, ttl:~d~%" url question ttl)
    
  
   (make-instance 'answer-response :name "sub1" :response (random 20)))


(defun fetch-from-sub-agents (question ttl)
  (format t "deferring to sub-agents question: ~a, ttl: ~d~%" question ttl)
  (if (has-sub-agents?)
      (mapcar (alexandria:curry #'fetch-from-sub-agent question ttl) 
              (alexandria:hash-table-values *sub-agents*))
      (list 
        (make-instance 'answer-response :name "sub1" :response 3)
        (make-instance 'answer-response :name "sub2" :response 13)
        (make-instance 'answer-response :name "sub3" :response 16))))
  
  
(defun summarize (sub-agent-responses)
    (make-instance 'answer-response 
                   :name (config "name" "spartacus")
                   :response (invoke-strategy 
                               (config "strategy" "average")
                               sub-agent-responses)
                   :children sub-agent-responses))


(defun formulate-response (question ttl)
  (-> (fetch-from-sub-agents question ttl)
      (summarize)))
  


(handler answers "^/answer$"
  (with-request-data (:parms ((question "q") (ttl "ttl")))
   		(json->response 
          (formulate-response question ttl))))
