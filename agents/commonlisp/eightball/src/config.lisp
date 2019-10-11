(in-package #:eightball)

(defvar *config* (make-hash-table :test #'equal))


(defun config! (&rest values)
  (when (and (first values) (second values))
    (setf (gethash (first values) *config*) (second values))))


(defun config (k &optional d)
  (gethash k *config* d))

(defun config-int (k &optional d)
  (parse-integer 
    (config k d)))


(defun strip-comments (s)
  (first (cl-strings:split s "#")))


(defun parse-kv (s)
   (-> (strip-comments s)
       (cl-strings:split "=")))


(defun process-config-line (line)
  (->> (parse-kv line)
       (mapcar #'cl-strings:clean)
       (apply #'config!)))

(defun load-config (f)
  (with-open-file (stream f)
    (loop for line = (read-line stream nil)
      while line do (process-config-line line))))
