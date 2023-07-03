(in-package #:org.tymoonnext.chirp)

(defvar *indentation-hints* (make-hash-table :test #'eq))

(defun define-indentation (symbol rule-form)
  (setf (gethash symbol *indentation-hints*)
        rule-form))

(defun initialize-slime ()
  (when (member "SWANK-INDENTATION" *modules* :test #'string=)
    (let* ((swank (find-package :swank))
           (tables (when swank (find-symbol (string '#:*application-hints-tables*) swank))))
      (when tables
        (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
        t))))

(initialize-slime)
