(in-package #:eightball)

(defun make-bearer (s)
  (format nil "Bearer ~A" s))

(defun host->url (s me)
  (format nil "~a/webhooks/~a" s me))


(defun make-uuid ()
  (print-object (uuid:make-v4-uuid) nil))


(defun handle-registration-response (host status)
  (case status
        (200 (format nil "registration succeeded: ~A~%" host))
        (T (format nil "registration failed: ~A, status: ~A~%" host status))))


(defun do-register (host uuid)
    (->> (make-bearer uuid)
         (list "Authorization")
         (list)
         (drakma:http-request host :method :put :additional-headers)
         (nth-value 1)
         (handle-registration-response host)))
    


(defun register-with-mothership (host me)
    (format t "registering with: ~A~%" host)
    (-> (host->url host me)
        (do-register (make-uuid))))