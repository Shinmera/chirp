#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *account/settings* "https://api.twitter.com/1.1/account/settings.json")
(defvar *account/verify-credentials* "https://api.twitter.com/1.1/account/verify_credentials.json")

(defclass* user ()
  (id screen-name contributors created-at
   counts language location notifications status profile
   follow-request-sent following entities
   geo translator protected verified
   time-zone url utc-offset)
  (:documentation "Class representation of a user."))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a #~d" (screen-name user) (id user)))
  user)

(defun make-user (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (let ((user (make-instance
                 'user
                 :id (param :id)
                 :screen-name (param :screen-name)
                 :contributors (param :contributors)
                 :created-at (parse-twitter-time (param :created-at))
                 :counts (list :statuses (param :statuses-count)
                               :listed (param :listed-count)
                               :friends (param :friends-count)
                               :followers (param :followers-count)
                               :favorites (param :favourites-count))
                 :language (param :lang)
                 :location (param :location)
                 :notifications (param :notifications)
                 :status (when (param :status)
                           (make-status (param :status)))
                 :entities (when (param :entities)
                             (make-entities (param :entities)))
                 :profile (make-profile parameters)
                 :follow-request-sent (param :follow-request-sent)
                 :following (param :following)
                 :geo (param :geo-enabled)
                 :translator (param :is-translator)
                 :protected (param :protected)
                 :verified (param :verified)
                 :time-zone (param :time-zone)
                 :utc-offset (param :utf-offset)
                 :url (param :url))))
      (setf (user (profile user)) user)
      user)))

(defclass* profile ()
  (user default name description show-inline-media
   background avatar colors)
  (:documentation "Class representation of the profile related settings of a user (avatar, background, colors)."))

(defmethod print-object ((profile profile) stream)
  (print-unreadable-object (profile stream :type T)
    (format stream "~a" (user profile)))
  profile)

(defun make-profile (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'profile
     :default (param :default-profile)
     :name (param :name)
     :description (param :description)
     :show-inline-media (param :show-inline-media)
     :background (list
                  :color (param :profile-background-color)
                  :use-image (param :profile-use-background-image)
                  :image-url (param :profile-background-image-url)
                  :image-url-https (param :profile-background-image-url-https)
                  :image-tile (param :profile-background-image-tile))
     :avatar (list
              :default (param :default-profile-image)
              :image-url (param :profile-image-url)
              :image-url-https (param :profile-image-url-https))
     :colors (list
              :link (param :profile-link-color)
              :text (param :profile-text-color)
              :sidebar-border (param :profile-sidebar-border-color)
              :sidebar-fill (param :profile-sidebar-fill-color)))))

(defclass* settings ()
  (force-https email-discoverable geo language protected screen-name
   show-inline-media sleep-time sleep-time-start sleep-time-end
   time-zone-name time-zone-info time-zone-offset cookie-personalization trend)
  (:documentation "Class representation of the twitter user settings object."))

(defun make-settings (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'settings
     :force-https (param :always-use-https)
     :email-discoverable (param :discoverable-by-email)
     :geo (param :geo-enabled)
     :language (param :language)
     :protected (param :protected)
     :screen-name (param :screen-name)
     :show-inline-media (param :show-all-inline-media)
     :sleep-time (cdr (assoc :enabled (param :sleep-time)))
     :sleep-time-start (cdr (assoc :start-time (param :sleep-time)))
     :sleep-time-end (cdr (assoc :end-time (param :sleep-time)))
     :time-zone-name (cdr (assoc :name (param :time-zone)))
     :time-zone-info (cdr (assoc :tzinfo-name (param :time-zone)))
     :time-zone-offset (cdr (assoc :utc-offset (param :time-zone)))
     :cookie-personalization (param :use-cookie-personalization)
     :trend (when (param :trend-location)
              (make-trend-location (param :trend-location))))))

(defun account/settings ()
  "Returns settings (including current trend, geo and sleep time information) for the authenticating user.

According to spec https://dev.twitter.com/docs/api/1.1/get/account/settings"
  (make-settings (signed-request *account/settings* :method :GET)))

(defun account/verify-credentials (&key include-entities (skip-status T))
  "Returns an HTTP 200 OK response code and a representation of the requesting user if authentication was successful; returns a 401 status code and an error message if not. Use this method to test if supplied user credentials are valid.

According to spec https://dev.twitter.com/docs/api/1.1/get/account/verify_credentials"
  (make-user (signed-request *account/verify-credentials* :parameters (prepare
                                                                       `(("include_entities" . ,include-entities)
                                                                         ("skip_status" . ,skip-status))) :method :GET)))

(defun account/self (&key include-entities (skip-status T))
  "Alias for ACCOUNT/VERIFY-CREDENTIALS."
  (account/verify-credentials :include-entities include-entities :skip-status skip-status))
