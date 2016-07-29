#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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

(defmethod print-object ((dm direct-message) stream)
  (print-unreadable-object (dm stream :type T)
    (format stream "~a -> ~a #~a" (screen-name (sender dm)) (screen-name (recipient dm)) (id dm)))
  dm)

(define-make-* (direct-message parameters)
  :id :text
  (:recipient (parse-when-param :recipient #'make-user))
  (:sender (parse-when-param :sender #'make-user))
  (:created-at (parse-when-param :created-at #'parse-twitter-time))
  (:entities (parse-when-param :entities #'make-entities)))

(defun direct-messages (&key since-id max-id (count 20) (include-entities T) skip-status (full-text T))
  "Returns the 20 most recent direct messages sent to the authenticating user. Includes detailed information about the sender and recipient user. You can request up to 200 direct messages per call, up to a maximum of 800 incoming DMs.

According to spec https://dev.twitter.com/docs/api/1.1/get/direct_messages"
  (assert (<= count 200) () "COUNT has to be less than 200.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-direct-message (signed-request *direct-messages/* :parameters (prepare* since-id max-id count include-entities skip-status full-text) :method :GET)))

(defun direct-messages/sent (&key since-id max-id (count 20) (page 0) (include-entities T) (full-text T))
  "Returns the 20 most recent direct messages sent by the authenticating user. Includes detailed information about the sender and recipient user. You can request up to 200 direct messages per call, up to a maximum of 800 outgoing DMs.

According to spec https://dev.twitter.com/docs/api/1.1/get/direct_messages/sent"
  (assert (<= count 200) () "COUNT has to be less than 200.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-direct-message (signed-request *direct-messages/sent* :parameters (prepare* since-id max-id count page include-entities full-text) :method :GET)))

(defun direct-messages/show (id &key (full-text T))
  "Returns a single direct message, specified by an id parameter. Like the direct-messages request, this method will include the user objects of the sender and recipient.

According to spec https://dev.twitter.com/docs/api/1.1/get/direct_messages/show"
  (make-direct-message (signed-request *direct-messages/show* :parameters (prepare* id full-text) :method :GET)))

(defun direct-messages/destroy (id &key (include-entities T))
  "Destroys the direct message specified in the required ID parameter. The authenticating user must be the recipient of the specified direct message.

According to spec https://dev.twitter.com/docs/api/1.1/post/direct_messages/destroy"
  (unless include-entities (setf include-entities "false"))
  (make-direct-message (signed-request *direct-messages/destroy* :parameters (prepare* id include-entities) :method :POST)))

(defun direct-messages/new (text &key user-id screen-name)
  "Sends a new direct message to the specified user from the authenticating user. Requires both the user and text parameters and must be a POST. Returns the sent message in the requested format if successful.

According to spec https://dev.twitter.com/docs/api/1.1/post/direct_messages/new"
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (make-direct-message (signed-request *direct-messages/new* :parameters (prepare* text user-id screen-name) :method :POST)))
