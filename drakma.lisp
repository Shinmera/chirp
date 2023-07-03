(in-package #:org.tymoonnext.chirp)

(defun perform-request (uri &rest params)
  (let ((drakma:*text-content-types* (cons '("application" . "json") (cons '("text" . "json") drakma:*text-content-types*))))
    (multiple-value-list (apply #'drakma:http-request uri
                                :external-format-in *external-format*
                                :external-format-out *external-format*
                                :url-encoder #'url-encode
                                params))))

(defun open-request (uri &rest params)
  (first (apply #'perform-request uri :want-stream T :close NIL params)))
