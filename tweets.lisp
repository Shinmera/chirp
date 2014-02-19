#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")

(defclass* tweet ()
  (id text created user source language parent parent-user
   location contributors hashtags urls symbols mentions
   retweeted favorited favorite-count retweet-count)
  (:documentation ""))

(defmethod print-object ((tweet tweet) stream)
  (print-unreadable-object (tweet stream :type T)
    (format stream "~a #~d" (user tweet) (id tweet)))
  tweet)

(defun make-tweet (json-data)
  )

(defun statuses/update (status &key reply-to lat lng place display-coordinates)
  (let ((parameters (prepare `(("status" . ,status)
                               ("in_reply_to_status_id" . ,reply-to)
                               ("lat" . ,lat)
                               ("long" . ,lng)
                               ("place_id" . ,place)
                               ("display_coordinates" . ,display-coordinates)
                               ("trim_user" . "T")))))
    (make-tweet (signed-request *statuses/update* :parameters parameters))))
