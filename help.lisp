#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *help/configuration* "https://api.twitter.com/1.1/help/configuration.json")
(defvar *help/languages* "https://api.twitter.com/1.1/help/languages.json")
(defvar *help/privacy* "https://api.twitter.com/1.1/help/privacy.json")
(defvar *help/tos* "https://api.twitter.com/1.1/help/tos.json")
(defvar *application/rate-limit-status* "https://api.twitter.com/1.1/application/rate_limit_status.json")
(defvar *cached-languages* ())
(defvar *cached-configuration*
  (make-instance 'configuration
                 :photo-size-limit 3145728
                 :short-url-length 23
                 :short-url-length-https 23
                 :non-username-paths '("about" "account" "accounts" "activity" "all" "announcements" "anywhere" "api_rules"
                                       "api_terms" "apirules" "apps" "auth" "badges" "blog" "business" "buttons" "contacts"
                                       "devices" "direct_messages" "download" "downloads" "edit_announcements" "faq"
                                       "favorites" "find_sources" "find_users" "followers" "following" "friend_request"
                                       "friendrequest" "friends" "goodies" "help" "home" "i" "im_account" "inbox"
                                       "invitations" "invite" "jobs" "list" "login" "logo" "logout" "me" "mentions"
                                       "messages" "mockview" "newtwitter" "notifications" "nudge" "oauth" "phoenix_search"
                                       "positions" "privacy" "public_timeline" "related_tweets" "replies"
                                       "retweeted_of_mine" "retweets" "retweets_by_others" "rules" "saved_searches"
                                       "search" "sent" "sessions" "settings" "share" "signup" "signin" "similar_to"
                                       "statistics" "terms" "tos" "translate" "trends" "tweetbutton" "twttr"
                                       "update_discoverability" "users" "welcome" "who_to_follow" "widgets" "zendesk_auth"
                                       "media_signup")
                 :max-media-per-upload 1
                 :characters-reserved-per-media 24
                 :dm-text-character-limit 10000))

(defclass* configuration ()
  (photo-size-limit photo-sizes short-url-length short-url-length-https
   non-username-paths max-media-per-upload characters-reserved-per-media
   dm-text-character-limit)
  (:documentation "Class representation of the twitter configuration object.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/configuration"))

(define-make-* (configuration parameters)
  :photo-size-limit :short-url-length :short-url-length-https
  :non-username-paths :max-media-per-upload :characters-reserved-per-media
  :dm-text-character-limit
  (:photo-sizes (loop for (type . params) in (cdr (assoc :photo-sizes parameters))
                      collect (cons type (make-entity type params)))))

(defclass* language ()
  (name code status)
  (:documentation "Class representation of the twitter language object.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/languages"))

(defmethod print-object ((language language) stream)
  (print-unreadable-object (language stream :type T)
    (format stream "~a" (name language)))
  language)

(define-make-* (language)
  :name :code :status)

(defclass* resource ()
  (address remaining reset limit)
  (:documentation "Class representation of a twitter resource objkect.

According to spec https://dev.twitter.com/docs/api/1.1/get/application/rate_limit_status"))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type T)
    (format stream "~a (~d/~d)" (address resource) (remaining resource) (limit resource)))
  resource)

(defun make-resource (parameters)
  (flet ((param (place) (cdr (assoc place (cdr parameters)))))
    (make-instance
     'resource
     :address (car parameters)
     :remaining (param :remaining)
     :reset (local-time:unix-to-timestamp (param :reset))
     :limit (param :limit))))

(defun help/configuration (&key refresh-cache)
  "Returns the current configuration used by Twitter including twitter.com slugs which are not usernames, maximum photo resolutions, and t.co URL lengths.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/configuration"
  (or (unless refresh-cache *cached-configuration*)
      (setf *cached-configuration* (make-configuration (signed-request *help/configuration* :method :GET)))))

(defun help/languages (&key refresh-cache)
  "Returns the list of languages supported by Twitter along with their ISO 639-1 code. The ISO 639-1 code is the two letter value to use if you include lang with any of your requests.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/languages"
  (or (unless refresh-cache *cached-languages*)
      (setf *cached-languages* (mapcar #'make-language (signed-request *help/languages* :method :GET)))))

(defun help/privacy ()
  "Returns Twitter's Privacy Policy.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/privacy"
  (cdr (assoc :privacy (signed-request *help/privacy* :method :GET))))

(defun help/tos ()
  "Returns the Twitter Terms of Service in the requested format. These are not the same as the Developer Rules of the Road.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/tos"
  (cdr (assoc :tos (signed-request *help/tos* :method :GET))))

(defun application/rate-limit-status (resources)
  "Returns the current rate limits for methods belonging to the specified resource families.

According to spec https://dev.twitter.com/docs/api/1.1/get/application/rate_limit_status"
  (setf resources (etypecase resources
                    (string resources)
                    (cons (format NIL "~{~a~^,~}" resources))))
  (let ((data (signed-request *application/rate-limit-status* :parameters `(("resources" . ,resources)) :method :GET)))
    (mapc #'(lambda (resource)
              (setf (cdr resource) (mapcar #'make-resource (cdr resource))))
          (cdr (assoc :resources data)))))

(defun valid-language-p (language &key refresh-cache)
  "Returns T if the given language code is a language covered by twitter.

See HELP/LANGUAGES."
  (when (or refresh-cache (not *cached-languages*))
    (help/languages :refresh-cache T))
  (loop for lang in *cached-languages*
        if (string-equal language (code lang))
          return T))
