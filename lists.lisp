#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *lists/list* "https://api.twitter.com/1.1/lists/list.json")
(defvar *lists/statuses* "https://api.twitter.com/1.1/lists/statuses.json")
(defvar *lists/show* "https://api.twitter.com/1.1/lists/show.json")
(defvar *lists/create* "https://api.twitter.com/1.1/lists/create.json")
(defvar *lists/update* "https://api.twitter.com/1.1/lists/update.json")
(defvar *lists/destroy* "https://api.twitter.com/1.1/lists/destroy.json")
(defvar *lists/members* "https://api.twitter.com/1.1/lists/members.json")
(defvar *lists/members/show* "https://api.twitter.com/1.1/lists/members/show.json")
(defvar *lists/members/create* "https://api.twitter.com/1.1/lists/members/create.json")
(defvar *lists/members/create-all* "https://api.twitter.com/1.1/lists/members/create_all.json")
(defvar *lists/members/destroy* "https://api.twitter.com/1.1/lists/destroy.json")
(defvar *lists/members/destroy-all* "https://api.twitter.com/1.1/lists/destroy_all.json")
(defvar *lists/subscribers* "https://api.twitter.com/1.1/lists/subscribers.json")
(defvar *lists/subscribers/show* "https://api.twitter.com/1.1/lists/subscribers/show.json")
(defvar *lists/subscribers/create* "https://api.twitter.com/1.1/lists/subscribers/create.json")
(defvar *lists/subscribers/destroy* "https://api.twitter.com/1.1/lists/subscribers/destroy.json")
(defvar *lists/subscriptions* "https://api.twitter.com/1.1/lists/subscriptions.json")
(defvar *lists/memberships* "https://api.twitter.com/1.1/lists/memberships.json")
(defvar *lists/ownerships* "https://api.twitter.com/1.1/lists/ownerships.json")

(defclass* user-list ()
  (id user name full-name description created-at uri counts mode following slug)
  (:documentation "Class representation of a twitter list object."))

(defmethod print-object ((list user-list) stream)
  (print-unreadable-object (list stream :type T)
    (format stream "~a #~a" (full-name list) (id list)))
  list)

(define-make-* (user-list parameters)
  :id :slug :name :full-name :description :uri :mode :following
  (:user (parse-when-param :user #'make-user))
  (:created-at (parse-when-param :created-at #'parse-twitter-time))
  (:counts `((:members . ,(cdr (assoc :member-count parameters)))
             (:subscribers . ,(cdr (assoc :subscriber-count parameters))))))

(defun lists/list (&key user-id screen-name reverse)
  "Returns all lists the authenticating or specified user subscribes to, including their own. The user is specified using the user_id or screen_name parameters. If no user is given, the authenticating user is used.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/list"
  (mapcar #'make-user-list (signed-request *lists/list* :parameters (prepare* user-id screen-name reverse) :method :GET)))

(defun lists/statuses (&key list-id slug owner-screen-name owner-id since-id max-id (count 20) (include-entities T) include-rts)
  "Returns a timeline of tweets authored by members of the specified list. Retweets are included by default. Use the include_rts=false parameter to omit retweets.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/statuses"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-status (signed-request *lists/statuses* :parameters (prepare* list-id slug owner-screen-name owner-id since-id max-id count include-entities include-rts) :method :GET)))

(defun lists/show (&key list-id slug owner-screen-name owner-id)
  "Returns the specified list. Private lists will only be shown if the authenticated user owns the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/show"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (make-user-list (signed-request *lists/show* :parameters (prepare* list-id slug owner-screen-name owner-id) :method :GET)))

(defun lists/create (name &key (mode :PUBLIC) description)
  "Creates a new list for the authenticated user. Note that you can't create more than 20 lists per account.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/create"
  (assert (member mode '(:PUBLIC :PRIVATE)) () "MODE must be one of (:PUBLIC :PRIVATE).")
  (make-user-list (signed-request *lists/create* :parameters (prepare* name mode description) :method :POST)))

(defun lists/update (&key list-id slug owner-screen-name owner-id name mode description)
  "Updates the specified list. The authenticated user must own the list to be able to update it.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/update"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (when mode (assert (member mode '(:PUBLIC :PRIVATE)) () "MODE must be one of (:PUBLIC :PRIVATE)."))
  (make-user-list (signed-request *lists/update* :parameters (prepare* list-id slug owner-screen-name owner-id name mode description) :method :POST)))

(defun lists/destroy (&key list-id slug owner-screen-name owner-id)
  "Deletes the specified list. The authenticated user must own the list to be able to destroy it.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/destroy"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (make-user-list (signed-request *lists/destroy* :parameters (prepare* list-id slug owner-id owner-screen-name) :method :POST)))

(defun lists/members (&key list-id slug owner-screen-name owner-id (include-entities T) skip-status)
  "Returns the members of the specified list. Private list members will only be shown if the authenticated user owns the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/members"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (unless include-entities (setf include-entities "false"))
  (map-cursor #'make-user :users *lists/members* :parameters (prepare* list-id slug owner-screen-name owner-id include-entities skip-status) :method :GET))

(defun lists/members/show (&key user-id screen-name list-id slug owner-screen-name owner-id (include-entities T) skip-status)
  "Check if the specified user is a member of the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/members/show"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (unless include-entities (setf include-entities "false"))
  (make-user (signed-request *lists/members/show* :parameters (prepare* user-id screen-name list-id slug owner-screen-name owner-id include-entities skip-status) :method :GET)))

(defun lists/members/create (&key user-id screen-name list-id slug owner-screen-name owner-id)
  "Add a member to a list. The authenticated user must own the list to be able to add members to it. Note that lists can't have more than 500 members.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/members/create"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (signed-request *lists/members/create* :parameters (prepare* user-id screen-name list-id slug owner-screen-name owner-id) :method :POST))

(defun lists/members/create-all (&key user-ids screen-names list-id slug owner-screen-name owner-id)
  "Adds multiple members to a list, by specifying a comma-separated list of member ids or screen names. The authenticated user must own the list to be able to add members to it. Note that lists can't have more than 5,000 members, and you are limited to adding up to 100 members to a list at a time with this method.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/members/create_all"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (assert (or user-ids screen-names) () "Either SCREEN-NAMES or USER-IDS are required.")
  (when user-ids (setf user-ids (format NIL "~{~a~^,~}" user-ids)))
  (when screen-names (setf screen-names (format NIL "~{~a~^,~}" screen-names)))
  (assert (and (<= (length user-ids) 100)
               (<= (length screen-names) 100)) () "A maximum of 100 users can be supplied by USER-IDS or SCREEN-NAMES.")
  (signed-request *lists/members/create-all* :parameters (prepare* (user-id . user-ids) (screen-name . screen-names) list-id slug owner-screen-name owner-id) :method :POST))

(defun lists/members/destroy (&key list-id slug owner-screen-name owner-id)
  "Unsubscribes the authenticated user from the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/subscribers/destroy"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (signed-request *lists/members/destroy* :parameters (prepare* list-id slug owner-screen-name owner-id) :method :POST))

(defun lists/members/destroy-all (&key user-ids screen-names list-id slug owner-screen-name owner-id)
  "Removes multiple members from a list, by specifying a comma-separated list of member ids or screen names. The authenticated user must own the list to be able to remove members from it. Note that lists can't have more than 500 members, and you are limited to removing up to 100 members to a list at a time with this method.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/members/destroy_all"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (assert (or user-ids screen-names) () "Either SCREEN-NAMES or USER-IDS are required.")
  (when user-ids (setf user-ids (format NIL "~{~a~^,~}" user-ids)))
  (when screen-names (setf screen-names (format NIL "~{~a~^,~}" screen-names)))
  (assert (and (<= (length user-ids) 100)
               (<= (length screen-names) 100)) () "A maximum of 100 users can be supplied by USER-IDS or SCREEN-NAMES.")
  (signed-request *lists/members/destroy-all* :parameters (prepare* (user-id . user-ids) (screen-name . screen-names) list-id slug owner-screen-name owner-id) :method :POST))

(defun lists/subscribers (&key list-id slug owner-screen-name owner-id (include-entities T) skip-status)
  "Returns the subscribers of the specified list. Private list subscribers will only be shown if the authenticated user owns the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/subscribers"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (unless include-entities (setf include-entities "false"))
  (map-cursor #'make-user :users *lists/subscribers* :parameters (prepare* list-id slug owner-screen-name owner-id include-entities skip-status)))

(defun lists/subscribers/show (&key user-id screen-name list-id slug owner-screen-name owner-id (include-entities T) skip-status)
  "Check if the specified user is a subscriber of the specified list. Returns the user if they are subscriber.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/subscribers/show"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (unless include-entities (setf include-entities "false"))
  (make-user (signed-request *lists/subscribers/show* :parameters (prepare* user-id screen-name list-id slug owner-screen-name owner-id include-entities skip-status) :method :GET)))

(defun lists/subscribers/create (&key list-id slug owner-screen-name owner-id)
  "Subscribes the authenticated user to the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/subscribers/create"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (make-user-list (signed-request *lists/subscribers/create* :parameters (prepare* list-id slug owner-screen-name owner-id) :method :POST)))

(defun lists/subscribers/destroy  (&key list-id slug owner-screen-name owner-id)
  "Unsubscribes the authenticated user from the specified list.

According to spec https://dev.twitter.com/docs/api/1.1/post/lists/subscribers/destroy"
  (assert (or list-id (and slug (or owner-screen-name owner-id))) () "Either LIST-ID or SLUG and OWNER-SCREEN-NAME or OWNER-ID are required.")
  (make-user-list (signed-request *lists/subscribers/destroy* :parameters (prepare* list-id slug owner-screen-name owner-id) :method :POST)))

(defun lists/subscriptions (&key user-id screen-name)
  "Obtain a collection of the lists the specified user is subscribed to, 20 lists per page by default. Does not include the user's own lists.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/subscriptions"
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (map-cursor #'make-user-list :lists *lists/subscriptions* :parameters (prepare* user-id screen-name (count . 1000))))

(defun lists/memberships (&key user-id screen-name)
  "Returns the lists the specified user has been added to. If user_id or screen_name are not provided the memberships for the authenticating user are returned.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/memberships"
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (map-cursor #'make-user-list :lists *lists/memberships* :parameters (prepare* user-id screen-name (count . 1000))))

(defun lists/ownerships (&key user-id screen-name)
  "Returns the lists owned by the specified Twitter user. Private lists will only be shown if the authenticated user is also the owner of the lists.

According to spec https://dev.twitter.com/docs/api/1.1/get/lists/ownerships"
  (assert (or user-id screen-name) () "Either SCREEN-NAME or USER-ID are required.")
  (map-cursor #'make-user-list :lists *lists/ownerships* :parameters (prepare* user-id screen-name (count . 1000))))
