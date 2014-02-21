#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *blocks/list* "https://api.twitter.com/1.1/blocks/list.json")
(defvar *blocks/ids* "https://api.twitter.com/1.1/blocks/ids.json")
(defvar *blocks/create* "https://api.twitter.com/1.1/blocks/create.json")
(defvar *blocks/destroy* "https://api.twitter.com/1.1/blocks/destroy.json")

(defun blocks/list (&key include-entities (skip-status T))
  "Returns a list of user objects that the authenticating user is blocking.

According to spec https://dev.twitter.com/docs/api/1.1/get/blocks/list"
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (map-cursor #'make-user :users *blocks/list* :parameters (prepare* include-entities skip-status)))

(defun blocks/ids ()
  "Returns a list of numeric user ids the authenticating user is blocking.

According to spec https://dev.twitter.com/docs/api/1.1/get/blocks/ids"
  (cursor-collect :ids *blocks/ids*))

(defun blocks/create (&key screen-name user-id include-entities (skip-status T))
  "Blocks the specified user from following the authenticating user. In addition the blocked user will not show in the authenticating users mentions or timeline (unless retweeted by another user). If a follow or friend relationship exists it is destroyed.

According to spec https://dev.twitter.com/docs/api/1.1/post/blocks/create"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *blocks/create* :parameters (prepare* screen-name user-id include-entities skip-status) :method :POST)))

(defun blocks/destroy (&key screen-name user-id include-entities (skip-status T))
  "Un-blocks the user specified in the ID parameter for the authenticating user. Returns the un-blocked user in the requested format when successful. If relationships existed before the block was instated, they will not be restored.

According to spec https://dev.twitter.com/docs/api/1.1/post/blocks/destroy"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *blocks/create* :parameters (prepare* screen-name user-id include-entities skip-status) :method :POST)))
