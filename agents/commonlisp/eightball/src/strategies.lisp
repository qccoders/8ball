(in-package #:eightball)


(defun invoke-strategy (name parms)
  (-> name
      (string-upcase)
      (find-symbol 'eightball)
      (funcall parms)))


(defun average (children)
  (->> children
       (mapcar (lambda (c) (slot-value c 'response)))
       (alexandria:mean)
       (round)))


(defun constant (children)
  (spy children)
  (config-int "constant.value"))


