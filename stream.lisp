#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *stream/statuses/filter* "https://stream.twitter.com/1.1/statuses/filter.json")
(defvar *stream/statuses/sample* "https://stream.twitter.com/1.1/statuses/sample.json")
(defvar *stream/statuses/firehose* "https://stream.twitter.com/1.1/statuses/firehose.json")
(defvar *stream/user* "https://userstream.twitter.com/1.1/user.json")
(defvar *stream/site* "https://sitestream.twitter.com/1.1/site.json")

(defclass message () ()
  (:documentation "Superclass for all stream messages.

According to spec https://dev.twitter.com/docs/streaming-apis/messages"))

(defclass* stream-status (message status)
  () (:documentation "Status notice"))

(defclass* stream-delete (message)
  (id user-id)
  (:documentation "Status deletion notice"))

(defclass* stream-scrub-geo (message)
  (user-id up-to-status-id)
  (:documentation "Location deletion notice"))

(defclass* stream-limit (message)
  (track)
  (:documentation "Limit notice"))

(defclass* stream-status-withheld (message)
  (id withheld-in-countries)
  (:documentation "Status withheld notice"))

(defclass* stream-user-withheld (message)
  (id withheld-in-countries)
  (:documentation "User withheld notice"))

(defclass* stream-disconnect (message)
  (code stream-name reason)
  (:documentation "Disconnection notice"))

(defclass* stream-friends (message)
  (friends)
  (:documentation "Friends list notice"))

(defclass* stream-event (message)
  (target source target-object created-at)
  (:documentation "Stream event superclass"))

(defclass* event-access-revoked (stream-event)
  () (:documentation "User deauthorize event"))

(defclass* event-block (stream-event)
  () (:documentation " event"))

(defclass* event-unblock (stream-event)
  () (:documentation " event"))

(defclass* event-favorite (stream-event)
  () (:documentation " event"))

(defclass* event-unfavorite (stream-event)
  () (:documentation " event"))

(defclass* event-follow (stream-event)
  () (:documentation " event"))

(defclass* event-unfollow (stream-event)
  () (:documentation " event"))

(defclass* event-list-create (stream-event)
  () (:documentation " event"))

(defclass* event-list-destroy (stream-event)
  () (:documentation " event"))

(defclass* event-list-update (stream-event)
  () (:documentation " event"))

(defclass* event-list-member-add (stream-event)
  () (:documentation " event"))

(defclass* event-list-member-remove (stream-event)
  () (:documentation " event"))

(defclass* event-list-user-subscribe (stream-event)
  () (:documentation " event"))

(defclass* event-list-user-unsubscribe (stream-event)
  () (:documentation " event"))

(defclass* event-user-update (stream-event)
  () (:documentation " event"))

(defclass* stream-warning (message)
  (code message user-id)
  (:documentation "Warning notice"))

(defclass* stream-control (message)
  (control-uri)
  (:documentation "Site stream control notice"))

(defclass* stream-envelope (message)
  (for-user message)
  (:documentation "Container class for site streams"))

(defun stream/user (handler-function &key delimited stall-warnings with replies track locations)
  "Streams messages for a single user, as described in User streams.

According to spec https://dev.twitter.com/docs/api/1.1/get/user
https://dev.twitter.com/docs/streaming-apis/streams/user
https://dev.twitter.com/docs/streaming-apis/messages"
  )
