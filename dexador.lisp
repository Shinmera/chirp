#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defun parameters->string (params)
  (with-output-to-string (out)
    (loop for cons on params
          for param = (car cons)
          do (format out "~a=~a" (url-encode (car param)) (url-encode (cdr param)))
             (when (cdr cons) (format out "&")))))

(defun convert-received-headers (headers)
  (loop for k being the hash-keys of headers
        for v being the hash-values of headers
        collect (cons (intern (string-upcase k) :keyword) v)))

(defun purify-form-data (params)
  (loop for param in params
        collect (if (listp (cdr param))
                    (cons (car param) (second param))
                    param)))

(defun perform-request (uri &key method parameters additional-headers form-data want-stream)
  (let ((params (quri:make-uri :query (parameters->string parameters)))
        (uri (quri:uri uri)))
    (multiple-value-bind (body return-code headers uri)
        (dexador:request (if form-data uri (quri:merge-uris params uri))
                         :method method
                         :headers additional-headers
                         :content (when form-data (purify-form-data parameters))
                         :want-stream want-stream)
      (list body return-code (convert-received-headers headers) uri))))

(defun open-request (uri &rest args)
  (first (apply #'perform-request uri :want-stream T args)))
