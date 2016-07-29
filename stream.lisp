#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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

(defclass* stream-unknown (message)
  (data) (:documentation "Unknown stream object"))

(defclass* stream-delete (message)
  (id user-id)
  (:documentation "Status deletion notice"))

(defmethod print-object ((message stream-delete) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "#~a" (id message)))
  message)

(define-make-* (stream-delete parameters)
  (:id (cdr (assoc :id (cdr (assoc :status parameters)))))
  (:user-id (cdr (assoc :user-id (cdr (assoc :status parameters))))))

(defclass* stream-scrub-geo (message)
  (user-id up-to-status-id)
  (:documentation "Location deletion notice"))

(defmethod print-object ((message stream-scrub-geo) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "->#~a" (up-to-status-id message)))
  message)

(define-make-* (stream-scrub-geo)
  :user-id :up-to-status-id)

(defclass* stream-limit (message)
  (track)
  (:documentation "Limit notice"))

(defmethod print-object ((message stream-limit) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "F:~a" (track message)))
  message)

(define-make-* (stream-limit)
  :track)

(defclass* stream-status-withheld (message)
  (id withheld-in-countries)
  (:documentation "Status withheld notice"))

(defmethod print-object ((message stream-status-withheld) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "#~a (~{~a~^, ~})" (id message) (withheld-in-countries message)))
  message)

(define-make-* (stream-status-withheld)
  :id :withheld-in-countries)

(defclass* stream-user-withheld (message)
  (id withheld-in-countries)
  (:documentation "User withheld notice"))

(defmethod print-object ((message stream-user-withheld) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "#~a (~{~a~^, ~})" (id message) (withheld-in-countries message)))
  message)

(define-make-* (stream-user-withheld)
  :id :withheld-in-countries)

(defclass* stream-disconnect (message)
  (code stream-name reason)
  (:documentation "Disconnection notice"))

(defmethod print-object ((message stream-disconnect) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "~a (~a) #~a" (reason message) (code message) (stream-name message)))
  message)

(define-make-* (stream-disconnect)
  :code :stream-name :reason)

(defclass* stream-friends (message)
  (friends)
  (:documentation "Friends list notice"))

(define-make-* (stream-friends)
  :friends)

(defclass* stream-event (message)
  (target source target-object created-at)
  (:documentation "Stream event superclass"))

(defmethod print-object ((message stream-event) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "~a -> ~a ~a" (screen-name (source message)) (screen-name (target message)) (target-object message)))
  message)

(defparameter *event->class-map*
  (alist-hash-table '(("access_revoked" . event-access-revoked)
                      ("block" . event-block)
                      ("unblock" . event-unblock)
                      ("favorite" . event-favorite)
                      ("unfavorite" . event-unfavorite)
                      ("follow" . event-follow)
                      ("unfollow" . event-unfollow)
                      ("list_created" . event-list-create)
                      ("list_destroyed" . event-list-destroy)
                      ("list_updated" . event-list-update)
                      ("list_member_added" . event-list-member-add)
                      ("list_member_removed" . event-list-member-remove)
                      ("list_user_subscribed" . event-list-user-subscribe)
                      ("list_user_unsubscribed" . event-list-user-unsubscribe)
                      ("user_update" . event-user-update)) :test 'equalp))

(defun make-stream-event (parameters)
  (let ((class (gethash (cdr (assoc :event parameters)) *event->class-map* 'event-unknown)))
    (make-instance
     class
     :target (make-user (cdr (assoc :target parameters)))
     :source (make-user (cdr (assoc :source parameters)))
     :target-object (cdr (assoc :target-object parameters))
     :created-at (parse-twitter-time (cdr (assoc :created-at parameters))))))

(defclass* event-unknown (stream-event)
  () (:documentation "Unknown stream event"))

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

(defmethod print-object ((message stream-warning) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "~a (~a)" (message message) (code message)))
  message)

(define-make-* (stream-warning)
  :code :message :user-id)

(defclass* stream-control (message)
  (control-uri)
  (:documentation "Site stream control notice"))

(defmethod print-object ((message stream-control) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "~a" (control-uri message)))
  message)

(define-make-* (stream-control)
  :control-uri)

(defclass* stream-envelope (message)
  (for-user message)
  (:documentation "Container class for site streams"))

(defmethod print-object ((message stream-envelope) stream)
  (print-unreadable-object (message stream :type T :identity T)
    (format stream "->#~a ~a" (for-user message) (message message)))
  message)

(define-make-* (stream-envelope parameters)
  :for-user
  (:message (make-stream-object (cdr (assoc :message parameters)))))

(defun make-stream-object (object)
  (flet ((p (param) (cdr (assoc param object))))
    (cond ((p :control) (make-stream-control (p :control)))
          ((p :warning) (make-stream-warning (p :warning)))
          ((p :disconnect) (make-stream-disconnect (p :disconnect)))
          ((p :user-withheld) (make-stream-user-withheld (p :user-withheld)))
          ((p :status-withheld) (make-stream-status-withheld (p :status-withheld)))
          ((p :limit) (make-stream-limit (p :limit)))
          ((p :scrub-geo) (make-stream-scrub-geo (p :scrub-geo)))
          ((p :delete) (make-stream-delete (p :delete)))
          ((p :for-user) (make-stream-envelope object))
          ((p :event) (make-stream-event object))
          ((p :friends) (make-stream-friends object))
          ((p :recipient) (make-direct-message object))
          ((p :text) (make-status object))
          ((p :direct-message) (make-direct-message (p :direct-message)))
          (T (make-instance 'stream-unknown :data object)))))

(defun parse-stream-line (line)
  (unless (string= "" line)
    (make-stream-object
     (yason:parse line :object-as :alist :object-key-fn #'to-keyword))))

(defun trim-whitespace (string)
  (string-trim '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space) string))

(defun stream/user (handler-function &key stall-warnings (filter-level :NONE) language (with :USER) replies count)
  "Streams messages for a single user, as described in User streams.
Each line is parsed into an appropriate object (NIL for empty lines) and passed to the handler function.
This is done as long as the handler function returns a non-NIL value. Once the handler returns NIL,
the loop is stopped and the stream is closed.

According to spec https://dev.twitter.com/docs/api/1.1/get/user
https://dev.twitter.com/docs/streaming-apis/streams/user
https://dev.twitter.com/docs/streaming-apis/messages"
  (assert (member filter-level '(:NONE :LOW :MEDIUM)) () "FILTER-LEVEL must be one of (:NONE :LOW :MEDIUM).")
  (assert (member with '(:USER :FOLLOWINGS)) () "WITH must be one of (:USER :FOLLOWINGS).")
  (when count (assert (< -150000 count 150000) () "COUNT must be NIL or between -150000 and 150000."))
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (when replies (setf replies "all"))
  (let ((stream (signed-stream-request *stream/user* :parameters (prepare* stall-warnings filter-level language with replies count) :method :GET)))
    (unwind-protect
         (loop for line = (read-line stream)
               for object = (parse-stream-line (trim-whitespace line))
               while (funcall handler-function object))
      (close stream))))

(defun stream/site (handler-function follow &key stall-warnings (filter-level :NONE) language (with :FOLLOW) replies count)
  "Streams messages for a set of users, as described in Site streams.
See STREAM/USER.

According to spec https://dev.twitter.com/docs/api/1.1/get/site
https://dev.twitter.com/docs/streaming-apis/streams/site
https://dev.twitter.com/docs/streaming-apis/messages"
  (setf follow (format NIL "~{~a~^,~}" follow))
  (assert (member filter-level '(:NONE :LOW :MEDIUM)) () "FILTER-LEVEL must be one of (:NONE :LOW :MEDIUM).")
  (assert (member with '(:USER :FOLLOWINGS)) () "WITH must be one of (:USER :FOLLOWINGS).")
  (when count (assert (< -150000 count 150000) () "COUNT must be NIL or between -150000 and 150000."))
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (when replies (setf replies "all"))
  (let ((stream (signed-stream-request *stream/site* :parameters (prepare* follow stall-warnings filter-level language with replies count) :method :GET)))
    (unwind-protect
         (loop for line = (read-line stream)
               for object = (parse-stream-line (trim-whitespace line))
               while (funcall handler-function object))
      (close stream))))

(defun stream/statuses/filter (handler-function &key follow track locations stall-warnings (filter-level :NONE) language count)
  "Returns public statuses that match one or more filter predicates. Multiple parameters may be specified which allows most clients to use a single connection to the Streaming API. Both GET and POST requests are supported, but GET requests with too many parameters may cause the request to be rejected for excessive URL length. Use a POST request to avoid long URLs.
See STREAM/USER.

According to spec https://dev.twitter.com/docs/api/1.1/post/statuses/filter
https://dev.twitter.com/docs/streaming-apis/messages"
  (setf follow (format NIL "~{~a~^,~}" follow))
  (setf track (format NIL "~{~a~^,~}" track))
  (setf locations (format NIL "~{~a~^,~}" locations))
  (assert (member filter-level '(:NONE :LOW :MEDIUM)) () "FILTER-LEVEL must be one of (:NONE :LOW :MEDIUM).")
  (when count (assert (< -150000 count 150000) () "COUNT must be NIL or between -150000 and 150000."))
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (let ((stream (signed-stream-request *stream/statuses/filter* :parameters (prepare* follow track locations stall-warnings filter-level language count) :method :GET)))
    (unwind-protect
         (loop for line = (read-line stream)
               for object = (parse-stream-line (trim-whitespace line))
               while (funcall handler-function object))
      (close stream))))

(defun stream/statuses/sample (handler-function &key stall-warnings (filter-level :NONE) language count)
  "Returns a small random sample of all public statuses. The Tweets returned by the default access level are the same, so if two different clients connect to this endpoint, they will see the same Tweets.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/sample
https://dev.twitter.com/docs/streaming-apis/messages"
  (assert (member filter-level '(:NONE :LOW :MEDIUM)) () "FILTER-LEVEL must be one of (:NONE :LOW :MEDIUM).")
  (when count (assert (< -150000 count 150000) () "COUNT must be NIL or between -150000 and 150000."))
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (let ((stream (signed-stream-request *stream/statuses/sample* :parameters (prepare* stall-warnings filter-level language count) :method :GET)))
    (unwind-protect
         (loop for line = (read-line stream)
               for object = (parse-stream-line (trim-whitespace line))
               while (funcall handler-function object))
      (close stream))))

(defun stream/statuses/firehose (handler-function &key stall-warnings (filter-level :NONE) language count)
  "Returns all public statuses. Few applications require this level of access. Creative use of a combination of other resources and various access levels can satisfy nearly every application use case.
This endpoint requires special permission to access.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/firehose
https://dev.twitter.com/docs/streaming-apis/messages"
  (assert (member filter-level '(:NONE :LOW :MEDIUM)) () "FILTER-LEVEL must be one of (:NONE :LOW :MEDIUM).")
  (when count (assert (< -150000 count 150000) () "COUNT must be NIL or between -150000 and 150000."))
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (let ((stream (signed-stream-request *stream/statuses/sample* :parameters (prepare* count stall-warnings filter-level language) :method :GET)))
    (unwind-protect
         (loop for line = (read-line stream)
               for object = (parse-stream-line (trim-whitespace line))
               while (funcall handler-function object))
      (close stream))))
