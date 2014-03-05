#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.chirp.doc
  (:use #:cl #:lquery #:lquery-doc)
  (:nicknames #:chirp-doc)
  (:export #:build-documentation))

(in-package #:org.tymoonnext.chirp.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :chirp))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :chirp :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :chirp)))))

