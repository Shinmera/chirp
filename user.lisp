#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *users/lookup* "https://api.twitter.com/1.1/users/lookup.json")
(defvar *users/show* "https://api.twitter.com/1.1/users/show.json")
(defvar *users/search* "https://api.twitter.com/1.1/users/search.json")
(defvar *users/contributees* "https://api.twitter.com/1.1/users/contributees.json")
(defvar *users/contributors* "https://api.twitter.com/1.1/users/contributors.json")
(defvar *users/profile-banner* "https://api.twitter.com/1.1/users/profile_banner.json")
(defvar *users/report-spam* "https://api.twitter.com/1.1/users/report_spam.json")

(defclass* user ()
  (id screen-name contributors created-at
   counts language location notifications status
   follow-request-sent following entities
   geo-enabled translator protected verified contributors-enabled
   time-zone url utc-offset
   default name description show-inline-media
   background avatar colors
   withheld-in-countries withheld-scope)
  (:documentation "Class representation of a user.

According to spec https://dev.twitter.com/docs/platform-objects/users"))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a #~d" (screen-name user) (id user)))
  user)

(define-make-* (user parameters)
  :id :screen-name :contributors :location :notifications
  :follow-request-sent :following :protected :verified :contributors-enabled
  :time-zone :utc-offset :url :name :location :description 
  :show-inline-media (:language . :lang) :geo-enabled
  :withheld-in-countries :withheld-scope
  (:translator . :is-translator) (:default . :default-profile)
  (:status (parse-when-param :status #'make-status))
  (:entities (parse-when-param :entities #'make-entities))
  (:created-at (parse-when-param :created-at #'parse-twitter-time))
  (:counts `((:statuses . ,(cdr (assoc :statuses-count parameters)))
             (:listed . ,(cdr (assoc :listed-count parameters)))
             (:friends . ,(cdr (assoc :friends-count parameters)))
             (:followers . ,(cdr (assoc :followers-count parameters)))
             (:favorites . ,(cdr (assoc :favourites-count parameters)))))
  (:background `((:color . ,(cdr (assoc :profile-background-color parameters)))
                 (:use-image . ,(cdr (assoc :profile-use-background-image parameters)))
                 (:image-url . ,(cdr (assoc :profile-background-image-url parameters)))
                 (:image-url-https . ,(cdr (assoc :profile-background-image-url-https parameters)))
                 (:image-tile . ,(cdr (assoc :profile-background-image-tile parameters)))))
  (:avatar `((:default . ,(cdr (assoc :default-profile-image parameters)))
             (:image-url . ,(cdr (assoc :profile-image-url parameters)))
             (:image-url-https . ,(cdr (assoc :profile-image-url-https parameters)))))
  (:colors `((:link . ,(cdr (assoc :profile-link-color parameters)))
             (:text . ,(cdr (assoc :profile-text-color parameters)))
             (:sidebar-border . ,(cdr (assoc :profile-sidebar-border-color parameters)))
             (:sidebar-fill . ,(cdr (assoc :profile-sidebar-fill-color parameters))))))

(defclass* banner ()
  (size width height url)
  (:documentation "Class representation of a banner as returned by users/profile-banner."))

(defmethod print-object ((banner banner) stream)
  (print-unreadable-object (banner stream :type T)
    (format stream "~a (~dx~d)" (size banner) (width banner) (height banner)))
  banner)

(define-make-* (banner parameters)
  (:size (car parameters))
  (:width  (cdr (assoc :w (cdr parameters))))
  (:height (cdr (assoc :h (cdr parameters))))
  (:url (cdr (assoc :url (cdr parameters)))))

(defun users/lookup (&key screen-names user-ids include-entities)
  "Returns fully-hydrated user objects for up to 100 users per request, as specified by the lists passed to the user_id and/or screen_name parameters.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/lookup"
  (assert (or screen-names user-ids) () "Either SCREEN-NAMES or USER-IDS are required.")
  (setf user-ids (format NIL "~{~a~^,~}" user-ids))
  (setf screen-names (format NIL "~{~a~^,~}" screen-names))
  (map-cursor #'make-user NIL *users/lookup* :parameters (prepare* user-ids screen-names include-entities) :method :POST))

(defun users/show (&key screen-name user-id include-entities)
  "Returns a variety of information about the user specified by the required user_id or screen_name parameter. The author's most recent Tweet will be returned inline when possible.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/show"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (make-user (signed-request *users/show* :parameters (prepare* screen-name user-id include-entities) :method :GET)))

(defun users/search (query &key (page 1) (count 5) include-entities)
  "Provides a simple, relevance-based search interface to public user accounts on Twitter. Try querying by topical interest, full name, company name, location, or other criteria. Exact match searches are not supported.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/search"
  (assert (< (* page count) 1000) () "Only the first 1000 results are available.")
  (mapcar #'make-user (signed-request *users/search* :parameters (prepare* (q . query) page count include-entities) :method :GET)))

(defun users/contributees (&key user-id screen-name include-entities (skip-status T))
  "Returns a list of users that the specified user can \"contribute\" to.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/contributees"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (mapcar #'make-user (signed-request *users/contributees* :parameters (prepare* user-id screen-name include-entities skip-status) :method :GET)))

(defun users/contributors (&key user-id screen-name include-entities (skip-status T))
  "Returns a list of users who can contribute to the specified account.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/contributors"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (mapcar #'make-user (signed-request *users/contributors* :parameters (prepare* user-id screen-name include-entities skip-status) :method :GET)))

(defun users/profile-banner (&key user-id screen-name)
  "Returns a map of the available size variations of the specified user's profile banner. If the user has not uploaded a profile banner, a HTTP 404 will be served instead. This method can be used instead of string manipulation on the profile_banner_url returned in user objects as described in User Profile Images and Banners.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/profile_banner"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (mapc #'(lambda (size) (setf (cdr size) (make-banner size)))
        (cdr (assoc :sizes (signed-request *users/profile-banner* :parameters (prepare* user-id screen-name) :method :GET)))))

(defun users/report-spam (&key user-id screen-name)
  "Report the specified user as a spam account to Twitter. Additionally performs the equivalent of POST blocks/create on behalf of the authenticated user.

According to spec https://dev.twitter.com/docs/api/1.1/post/users/report_spam"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (make-user (signed-request *users/report-spam* :parameters (prepare* user-id screen-name) :method :GET)))
