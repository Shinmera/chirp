#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defun perform-request (uri &rest params)
  (let ((drakma:*text-content-types* (cons '("application" . "json") (cons '("text" . "json") drakma:*text-content-types*))))
    (multiple-value-list (apply #'drakma:http-request uri
                                :external-format-in *external-format*
                                :external-format-out *external-format*
                                :url-encoder #'url-encode
                                params))))

(defun open-request (uri &rest params)
  (apply #'perform-request uri :want-stream T :close NIL params))
