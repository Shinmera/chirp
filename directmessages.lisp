#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *direct-messages/* "https://api.twitter.com/1.1/direct_messages.json")
(defvar *direct-messages/sent* "https://api.twitter.com/1.1/direct_messages/sent.json")
(defvar *direct-messages/show* "https://api.twitter.com/1.1/direct_messages/show.json")
(defvar *direct-messages/destroy* "https://api.twitter.com/1.1/direct_messages/destroy.json")
(defvar *direct-messages/new* "https://api.twitter.com/1.1/direct_messages/new.json")

(defclass* direct-message ()
  (id text recipient sender created-at entities)
  (:documentation "Class representation of a twitter direct-message object."))

(define-make-* (direct-message parameters)
  :id :text
  (:recipient (parse-when-param :recipient #'make-user))
  (:sender (parse-when-param :sender #'make-user))
  (:created-at (parse-when-param :created-at #'parse-twitter-time))
  (:entities (parse-when-param :entities #'make-entities)))

(defun direct-messages (&key since-id max-id (count 20) (include-entities T) skip-status)
  "Returns the 20 most recent direct messages sent to the authenticating user. Includes detailed information about the sender and recipient user. You can request up to 200 direct messages per call, up to a maximum of 800 incoming DMs.

According to spec https://dev.twitter.com/docs/api/1.1/get/direct_messages"
  (assert (<= count 200) () "COUNT has to be less than 200.")
  (mapcar #'make-direct-message (signed-request *direct-messages/* :parameters (prepare* since-id max-id count include-entities skip-status) :method :GET)))

(defun direct-messages/sent ()
  "

According to spec "
  )

(defun direct-messages/show ()
  "

According to spec "
  )

(defun direct-messages/destroy ()
  "

According to spec "
  )

(defun direct-messages/new ()
  "

According to spec "
  )
