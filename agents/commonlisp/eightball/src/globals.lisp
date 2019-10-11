(in-package #:eightball)


(defvar *init-handlers* nil)
(defvar *handlers* (make-hash-table :test #'equal))


(defun spy (s)
  (format t "~a~%" s) s)

(defun string-after-prefix (s p)
  (-> (alexandria:starts-with-subseq p s :return-suffix t)
      (multiple-value-list)
      (second)))


(defun string->json (s)
	(json:with-decoder-simple-clos-semantics
		(let ((json:*json-symbols-package* (find-package :cf-native-lisp)))
			   (json:decode-json-from-string s))))


(defun o->json-string (j)
	(with-output-to-string (*standard-output*)
		(json:encode-json j)))
