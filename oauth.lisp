#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defconstant +oauth/request-token+ "https://dev.twitter.com/docs/api/1/post/oauth/request_token")


(defun oauth-response->alist (body)
  (mapcar #'(lambda (assignment)
              (let ((vars (split-sequence #\= assignment)))
                (setf (cdr vars) (cadr vars)) vars))
          (split-sequence #\& body)))

(defun pin-request-token ()
  (multiple-value-bind (body status headers)
      (drakma:http-request +oauth/request-token+ :method :POST :parameters '(("oauth_callback" . "oob")))
    (if (= 200 status)
        (oauth-response->alist body)
        (error))))

