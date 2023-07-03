(in-package #:org.tymoonnext.chirp)

(defvar *connection-values* '(:following :following-requested :followed-by :blocking :none)
  "Possible values for the connections field in a relationship object.")

(defvar *friends/ids* "https://api.twitter.com/1.1/friends/ids.json")
(defvar *friends/list* "https://api.twitter.com/1.1/friends/list.json")
(defvar *followers/ids* "https://api.twitter.com/1.1/followers/ids.json")
(defvar *followers/list* "https://api.twitter.com/1.1/followers/list.json")
(defvar *friendships/incoming* "https://api.twitter.com/1.1/friendships/incoming.json")
(defvar *friendships/outgoing* "https://api.twitter.com/1.1/friendships/outgoing.format")
(defvar *friendships/create* "https://api.twitter.com/1.1/friendships/create.json")
(defvar *friendships/destroy* "https://api.twitter.com/1.1/friendships/destroy.json")
(defvar *friendships/update* "https://api.twitter.com/1.1/friendships/update.json")
(defvar *friendships/show* "https://api.twitter.com/1.1/friendships/show.json")
(defvar *friendships/lookup* "https://api.twitter.com/1.1/friendships/lookup.json")
(defvar *friendships/no-retweets/ids* "https://api.twitter.com/1.1/friendships/no_retweets/ids.json")

(defclass* relationship ()
  (id screen-name name followed-by following can-dm blocking all-replies want-retweets marked-spam notifications-enabled connections)
  (:documentation "Class representation of a relationship (target or source) to a user."))

(defmethod print-object ((relationship relationship) stream)
  (print-unreadable-object (relationship stream :type T)
    (format stream "~a ~@[(~{~a~^, ~})~]" (screen-name relationship) (connections relationship)))
  relationship)

(define-make-* (relationship parameters)
  :id :followed-by :screen-name :following :can-dm :blocking :all-replies :want-retweets :marked-spam :notifications-enabled
  (:connections (when-let ((connections (cdr (assoc :connections parameters))))
                  (mapcar #'to-keyword connections))))

(defun friends/ids (&key user-id screen-name)
  "Returns a list of user IDs for every user the specified user is following (otherwise known as their \"friends\").

According to spec https://dev.twitter.com/docs/api/1.1/get/friends/ids"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (cursor-collect :ids *friends/ids* :parameters (prepare* user-id screen-name ("count" . "5000")) :method :GET))

(defun friends/list (&key user-id screen-name (skip-status T) (include-user-entities T))
  "Returns a list of user objects for every user the specified user is following (otherwise known as their \"friends\").

According to spec https://dev.twitter.com/docs/api/1.1/get/friends/list"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (unless skip-status (setf skip-status "false"))
  (unless include-user-entities (setf include-user-entities "false"))
  (map-cursor #'make-user :users *friends/list* :parameters (prepare* user-id screen-name skip-status include-user-entities ("count" . "5000")) :method :GET))

(defun followers/ids (&key user-id screen-name)
  "Returns a list of user IDs for every user following the specified user.

According to spec https://dev.twitter.com/docs/api/1.1/get/followers/ids"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (cursor-collect :ids *followers/ids* :parameters (prepare* user-id screen-name ("count" . "5000")) :method :GET))

(defun followers/list (&key user-id screen-name (skip-status T) (include-user-entities T))
  "Returns a list of user objects for users following the specified user.

According to spec https://dev.twitter.com/docs/api/1.1/get/followers/list"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (unless skip-status (setf skip-status "false"))
  (unless include-user-entities (setf include-user-entities "false"))
  (map-cursor #'make-user :users *followers/list* :parameters (prepare* user-id screen-name skip-status include-user-entities ("count" . "5000")) :method :GET))

(defun friendships/incoming ()
  "Returns a collection of numeric IDs for every user who has a pending request to follow the authenticating user.

According to spec https://dev.twitter.com/docs/api/1.1/get/friendships/incoming"
  (cursor-collect :ids *friendships/incoming* :method :GET))

(defun friendships/outgoing ()
  "Returns a collection of numeric IDs for every protected user for whom the authenticating user has a pending follow request.

According to spec https://dev.twitter.com/docs/api/1.1/get/friendships/outgoing"
  (cursor-collect :ids *friendships/outgoing* :method :GET))

(defun friendships/create (&key screen-name user-id follow)
  "Allows the authenticating users to follow the user specified in the ID parameter.

According to spec https://dev.twitter.com/docs/api/1.1/post/friendships/create"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (make-user (signed-request *friendships/create* :parameters (prepare* screen-name user-id follow) :method :POST)))

(defun friendships/destroy (&key screen-name user-id)
  "Allows the authenticating user to unfollow the user specified in the ID parameter.

According to spec https://dev.twitter.com/docs/api/1.1/post/friendships/destroy"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (make-user (signed-request *friendships/destroy* :parameters (prepare* screen-name user-id) :method :POST)))

(defun friendships/update (&key screen-name user-id (device NIL d-p) (retweets NIL r-p))
  "Allows one to enable or disable retweets and device notifications from the specified user.
The first return value is the relationship object \"target\", the second \"source\".

According to spec https://dev.twitter.com/docs/api/1.1/post/friendships/update"
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (if d-p (setf device (if device "true" "false")))
  (if r-p (setf retweets (if retweets "true" "false")))
  (let ((data (signed-request *friendships/update* :parameters (prepare* screen-name user-id device retweets) :method :POST)))
    (values (make-relationship (cdr (assoc :target data)))
            (make-relationship (cdr (assoc :source data))))))

(defun friendships/show (&key source-screen-name source-user-id target-screen-name target-user-id)
  "Returns detailed information about the relationship between two arbitrary users.
The first return value is the relationship object \"target\", the second \"source\".

According to spec https://dev.twitter.com/docs/api/1.1/get/friendships/show"
  (assert (or target-user-id target-screen-name) () "Either TARGET-USER-ID or TARGET-SCREEN-NAME are required.")
  (assert (or source-user-id source-screen-name) () "Either SOURCE-USER-ID or SOURCE-SCREEN-NAME are required.")
  (let ((data (signed-request *friendships/show* :parameters (prepare* target-screen-name target-user-id source-screen-name source-user-id) :method :POST)))
    (values (make-relationship (cdr (assoc :target data)))
            (make-relationship (cdr (assoc :source data))))))

(defun friendships/lookup (&key screen-names user-ids)
  "Returns the relationships of the authenticating user to the list of up to 100 screen-names or user-ids provided.

According to spec https://dev.twitter.com/docs/api/1.1/get/friendships/lookup"
  (assert (or user-ids screen-names) () "Either USER-ID or SCREEN-NAME are required.")
  (when user-ids (setf user-ids (format NIL "~{~a~^,~}" user-ids)))
  (when screen-names (setf screen-names (format NIL "~{~a~^,~}" screen-names)))
  (mapcar #'make-relationship (signed-request *friendships/lookup* :parameters (prepare* (screen-name . screen-names) (user-id . user-ids)) :method :GET)))

(defun friendships/no-retweets/ids ()
  "Returns a collection of user_ids that the currently authenticated user does not want to receive retweets from.

According to spec https://dev.twitter.com/docs/api/1.1/get/friendships/no_retweets/ids"
  (signed-request *friendships/no-retweets/ids* :method :GET))
