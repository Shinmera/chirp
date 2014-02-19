#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")

(defclass* status ()
  (id text created user source language parent parent-user
   location contributors hashtags urls symbols mentions
   retweeted favorited favorite-count retweet-count)
  (:documentation ""))

(defmethod print-object ((status status) stream)
  (print-unreadable-object (status stream :type T)
    (format stream "~a #~d" (user status) (id status)))
  status)

(defun make-status (parameters)
  )

(defun statuses/update (status &key reply-to lat lng place display-coordinates)
  (let ((parameters (prepare `(("status" . ,status)
                               ("in_reply_to_status_id" . ,reply-to)
                               ("lat" . ,lat)
                               ("long" . ,lng)
                               ("place_id" . ,place)
                               ("display_coordinates" . ,display-coordinates)
                               ("trim_user" . "T")))))
    (make-status (signed-request *statuses/update* :parameters parameters))))
