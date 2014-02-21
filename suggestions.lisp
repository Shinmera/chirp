#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *users/suggestions/slug* "https://api.twitter.com/1.1/users/suggestions/~a.json")
(defvar *users/suggestions* "https://api.twitter.com/1.1/users/suggestions.json")
(defvar *users/suggestions/slug/members* "https://api.twitter.com/1.1/users/suggestions/~a/members.json")

(defclass* slug ()
  (name slug size users)
  (:documentation "Class representation of a twitter suggestion (slug) object."))

(defmethod print-object ((slug slug) stream)
  (print-unreadable-object (slug stream :type T)
    (format stream "~a" (name slug)))
  slug)

(define-make-* (slug parameters)
  :name :slug :size
  (:users (when-let ((users (cdr (assoc :users parameters))))
            (mapcar #'make-user users))))

(defun users/suggestions/slug (slug &key language)
  "Access the users in a given category of the Twitter suggested user list.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/suggestions/%3Aslug"
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (make-slug (signed-request (format NIL *users/suggestions/slug* slug) :parameters (prepare* (lang . language)) :method :GET)))

(defun users/suggestions (&key language)
  "Access to Twitter's suggested user list. This returns the list of suggested user categories. The category can be used in GET users/suggestions/:slug to get the users in that category.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/suggestions"
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (mapcar #'make-slug (signed-request *users/suggestions* :parameters (prepare* (lang . language)) :method :GET)))

(defun users/suggestions/slug/members (slug)
  "Access the users in a given category of the Twitter suggested user list and return their most recent status if they are not a protected user.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/suggestions/%3Aslug/members"
  (mapcar #'make-user (signed-request (format NIL *users/suggestions/slug/members* slug) :method :GET)))

(defgeneric save (object)
  (:documentation "Save the given object to twitter. 
Does not guarantee to save every attribute, see the individual functions."))

(defmethod save ((settings settings))
  "Save the account settings. Returns a new settings object as per ACCOUNT/SETTINGS/POST."
  (account/settings/post (when (trend settings) (woeid (trend settings)))
                         (sleep-time settings) (sleep-time-start settings) (sleep-time-end settings)
                         (time-zone settings) (language settings)))
