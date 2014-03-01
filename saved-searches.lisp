#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *saved-searches/list* "https://api.twitter.com/1.1/saved_searches/list.json")
(defvar *saved-searches/show/id* "https://api.twitter.com/1.1/saved_searches/show/~a.json")
(defvar *saved-searches/create* "https://api.twitter.com/1.1/saved_searches/create.json")
(defvar *saved-searches/destroy/id* "https://api.twitter.com/1.1/saved_searches/destroy/~a.json")

(defclass* saved-search ()
  (id created-at name search-position query)
  (:documentation "Class representation of a twitter saved-search object."))

(defmethod print-object ((search saved-search) stream)
  (print-unreadable-object (search stream :type T)
    (format stream "~a #~a" (query search) (id search)))
  search)

(define-make-* (saved-search)
  :id :created-at :name :query
  (:search-position . :position))

(defun saved-searches/list ()
  "Returns the authenticated user's saved search queries.

According to spec https://dev.twitter.com/docs/api/1.1/get/saved_searches/list"
  (mapcar #'make-saved-search (signed-request *saved-searches/list* :method :GET)))

(defun saved-searches/show/id (id)
  "Retrieve the information for the saved search represented by the given id. The authenticating user must be the owner of saved search ID being requested.

According to spec https://dev.twitter.com/docs/api/1.1/get/saved_searches/show/%3Aid"
  (make-saved-search (signed-request (format NIL *saved-searches/show/id* id) :method :GET)))

(defun saved-searches/create (query)
  "Create a new saved search for the authenticated user. A user may only have 25 saved searches.

According to spec https://dev.twitter.com/docs/api/1.1/post/saved_searches/create"
  (make-saved-search (signed-request *saved-searches/create* :parameters `(("query" . ,query)) :method :POST)))

(defun saved-searches/destroy/id (id)
  "Destroys a saved search for the authenticating user. The authenticating user must be the owner of saved search id being destroyed.

According to spec https://dev.twitter.com/docs/api/1.1/post/saved_searches/destroy/%3Aid"
  (make-saved-search (signed-request (format NIL *saved-searches/destroy/id* id) :method :POST)))
