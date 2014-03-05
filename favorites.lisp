#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *favorites/list* "https://api.twitter.com/1.1/favorites/list.json")
(defvar *favorites/destroy* "https://api.twitter.com/1.1/favorites/destroy.json")
(defvar *favorites/create* "https://api.twitter.com/1.1/favorites/create.json")

(defun favorites/list (&key user-id screen-name (count 20) since-id max-id (include-entities T))
  "Returns the 20 most recent Tweets favorited by the authenticating or specified user.

According to spec https://dev.twitter.com/docs/api/1.1/get/favorites/list"
  (assert (<= count 200) () "COUNT must be less than or equal to 200.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-status (signed-request *favorites/list* :parameters (prepare* user-id screen-name count since-id max-id include-entities) :method :GET)))

(defun favorites/destroy (id &key (include-entities T))
  "Un-favorites the status specified in the ID parameter as the authenticating user. Returns the un-favorited status in the requested format when successful.

According to spec https://dev.twitter.com/docs/api/1.1/post/favorites/destroy"
  (unless include-entities (setf include-entities "false"))
  (make-status (signed-request *favorites/destroy* :parameters (prepare* id include-entities) :method :POST)))

(defun favorites/create (id &key (include-entities T))
  "Favorites the status specified in the ID parameter as the authenticating user. Returns the favorite status when successful.

According to spec https://dev.twitter.com/docs/api/1.1/post/favorites/create"
  (unless include-entities (setf include-entities "false"))
  (make-status (signed-request *favorites/create* :parameters (prepare* id include-entities) :method :POST)))
