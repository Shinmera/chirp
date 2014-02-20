#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *account/settings* "https://api.twitter.com/1.1/account/settings.json")
(defvar *account/verify-credentials* "https://api.twitter.com/1.1/account/verify_credentials.json")
(defvar *account/update-profile* "https://api.twitter.com/1.1/account/update_profile.json")
(defvar *account/update-profile-background-image* "https://api.twitter.com/1.1/account/update_profile_background_image.json")
(defvar *account/update-profile-colors* "https://api.twitter.com/1.1/account/update_profile_colors.json")
(defvar *account/update-profile-image* "https://api.twitter.com/1.1/account/update_profile_image.json")
(defvar *account/update-profile-banner* "https://api.twitter.com/1.1/account/update_profile_banner.json")
(defvar *account/remove-profile-banner* "https://api.twitter.com/1.1/account/remove_profile_banner.json")

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
   background avatar colors location)
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
     :location (param :location)
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
  (make-user (signed-request *account/verify-credentials* :parameters (prepare* include-entities skip-status) :method :GET)))

(defun account/self (&key include-entities (skip-status T))
  "Alias for ACCOUNT/VERIFY-CREDENTIALS."
  (account/verify-credentials :include-entities include-entities :skip-status skip-status))

(defun account/settings/post (trend-woeid sleep-time sleep-time-start sleep-time-end time-zone language)
  "Updates the authenticating user's settings. Returns a new settings object.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/settings"
  (flet ((format-time (a)
           (etypecase a
             (string a)
             (local-time:timestamp (local-time:format-timestring NIL a :format '((:hour 2) #\- (:min 2)))))))
    (when sleep-time (setf sleep-time "true"))
    (when sleep-time-start (setf sleep-time-start (format-time sleep-time-start)))
    (when sleep-time-end (setf sleep-time-end (format-time sleep-time-end)))
    (assert (valid-language-p language) () "~a is not a supported language." language)
    (make-settings (signed-request *account/settings* :parameters (prepare* (trend-location-woeid . trend-woeid)
                                                                            (sleep-time-enabled . sleep-time)
                                                                            (start-sleep-time . sleep-time-start)
                                                                            (end-sleep-time . sleep-time-end)
                                                                            time-zone (lang . language))))))

(defun account/update-profile (&key name url location description include-entities (skip-status T))
  "Sets values that users are able to set under the \"Account\" tab of their settings page. Only the parameters specified will be updated.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/update_profile"
  (when name (assert (< (length name) 21) () "Name must be less than 21 characters long."))
  (when url (assert (< (length url) 101) () "URL must be less than 101 characters long."))
  (when location (assert (< (length location) 31) () "Location must be less than 31 characters long."))
  (when description (assert (< (length description) 161)) () "Description must be less than 161 characters long.")
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *account/update-profile* :parameters (prepare* name url location description include-entities skip-status) :method :POST)))

(defun account/update-profile-background-image (&key image (tile NIL t-v-p) (use-image NIL i-v-p) include-entities (skip-status T))
  "Updates the authenticating user's profile background image. This method can also be used to enable or disable the profile background image.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/update_profile_background_image"
  (setf image (etypecase image
                (null)
                ((array (unsigned-byte 8) (*)) (base64:usb8-array-to-base64-string image))
                (pathname (file-to-base64-string image))))
  (when t-v-p (setf tile (if tile "true" "false")))
  (when i-v-p (setf use-image (if use-image "true" "false")))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *account/update-profile-background-image* :parameters (prepare* image tile use-image include-entities skip-status) :method :POST)))

(defun account/update-profile-colors (&key background-color link-color sidebar-border-color sidebar-fill-color text-color include-entities (skip-status T))
  "Sets one or more hex values that control the color scheme of the authenticating user's profile page on twitter.com. Each parameter's value must be a valid hexidecimal value, and may be either three or six characters (ex: #fff or #ffffff).

According to spec https://dev.twitter.com/docs/api/1.1/post/account/update_profile_colors"
  (macrolet ((assert-color (var) `(assert (or (= (length ,var) 3) (= (length ,var) 6)) () ,(format NIL "~a must be 3 or 6 characters long." var))))
    (assert-color background-color)
    (assert-color link-color)
    (assert-color sidebar-border-color)
    (assert-color sidebar-fill-color)
    (assert-color text-color))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *account/update-profile-colors* :parameters (prepare* background-color link-color sidebar-border-color
                                                                                   sidebar-fill-color text-color
                                                                                   include-entities skip-status) :method :POST)))

(defun account/update-profile-image (image &key include-entities (skip-status T))
  "Updates the authenticating user's profile image. Note that this method expects raw multipart data, not a URL to an image.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/update_profile_image"
  (setf image (etypecase image
                ((array (unsigned-byte 8) (*)) (base64:usb8-array-to-base64-string image))
                (pathname (file-to-base64-string image))))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (make-user (signed-request *account/update-profile-image* :parameters (prepare* image include-entities skip-status) :method :POST)))

(defun account/remove-profile-banner ()
  "Removes the uploaded profile banner for the authenticating user. Returns T on success.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/remove_profile_banner"
  (signed-request *account/remove-profile-banner* :method :POST)
  T)

(defun account/update-profile-banner (image &key width height offset-left offset-top)
  "Uploads a profile banner on behalf of the authenticating user. For best results, upload an <3MB image that is exactly 1252px by 626px. Returns T on success.

According to spec https://dev.twitter.com/docs/api/1.1/post/account/update_profile_banner"
  (assert (or (not (or width height offset-left offset-top))
              (and width height offset-left offset-top))
          () "You must either provide all of the keyword parameters or none.")
  (setf image (etypecase image
                ((array (unsigned-byte 8) (*)) (base64:usb8-array-to-base64-string image))
                (pathname (file-to-base64-string image))))
  (signed-request *account/update-profile-banner* :parameters (prepare* (banner . image) width height offset-left offset-top) :method :POST)
  T)

(defgeneric save (object)
  (:documentation "Save the given object to twitter. 
Does not guarantee to save every attribute, see the individual functions."))

(defmethod save ((settings settings))
  "Save the account settings. Returns a new settings object as per ACCOUNT/SETTINGS/POST."
  (account/settings/post (when (trend settings) (woeid (trend settings)))
                         (sleep-time settings) (sleep-time-start settings) (sleep-time-end settings)
                         (time-zone settings) (language settings)))

(defmethod save ((profile profile))
  "Save the profile settings including profile theming. Returns a new profile object as per ACCOUNT/UPDATE-PROFILE."
  (profile (account/update-profile :name (name profile) :url (url profile) :location (location profile) :description (description profile))))

(defmethod save ((user user))
  "Save the account and profile settings. Returns a new profile object as per ACCOUNT/UPDATE-PROFILE.")
