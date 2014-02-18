#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")

(defun prepare (parameters)
  (mapc #'(lambda (pair)
            (unless (stringp (cdr pair))
              (setf (cdr pair) (write-to-string (cdr pair)))))
        (delete () parameters :key #'cdr)))

(defun status-update (status &key reply-to lat lng place display-coordinates)
  (let ((parameters (prepare `(("status" . ,status)
                               ("in_reply_to_status_id" . ,reply-to)
                               ("lat" . ,lat)
                               ("long" . ,lng)
                               ("place_id" . ,place)
                               ("display_coordinates" . ,display-coordinates)
                               ("trim_user" . "T")))))
    (signed-request *statuses/update* :parameters parameters)))
