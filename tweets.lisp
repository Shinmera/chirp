#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")

(defclass tweet ()
  ((%id :initarg :id :accessor id)
   (%text :initarg :text :accessor text)
   (%created :initarg :created :accessor created)
   (%user :initarg :user :accessor user)
   (%source :initarg :source :accessor source)
   (%language :initarg :language :accessor language)
   (%parent :initarg :parent :accessor parent)
   (%parent-user :initarg :parent-user :accessor parent-user)
   (%location :initarg :location :accessor location)
   (%contributors :initarg :contributors :accessor contributors)
   (%hashtags :initarg :hashtags :accessor hashtags)
   (%urls :initarg :urls :accessor urls)
   (%symbols :initarg :symbols :accessor symbols)
   (%mentions :initarg :mentions :accessor mentions)
   (%retweeted :initarg :retweeted :accessor retweeted)
   (%favorited :initarg :favorited :accessor favorited)
   (%favorite-count :initarg :favorite-count :accessor favorite-count)
   (%retweet-count :initarg :retweet-count :accessor retweet-count))
  (:documentation ""))

(defmethod print-object ((tweet tweet) stream)
  (print-unreadable-object (tweet stream :type T)
    (format stream "~a #~d" (user tweet) (id tweet)))
  tweet)

(defun make-tweet (json-data)
  )

(defun status-update (status &key reply-to lat lng place display-coordinates)
  (let ((parameters (prepare `(("status" . ,status)
                               ("in_reply_to_status_id" . ,reply-to)
                               ("lat" . ,lat)
                               ("long" . ,lng)
                               ("place_id" . ,place)
                               ("display_coordinates" . ,display-coordinates)
                               ("trim_user" . "T")))))
    (make-tweet (signed-request *statuses/update* :parameters parameters))))
