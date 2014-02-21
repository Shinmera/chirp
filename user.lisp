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
(defvar *blocks/list* "https://api.twitter.com/1.1/blocks/list.json")
(defvar *blocks/ids* "https://api.twitter.com/1.1/blocks/ids.json")
(defvar *blocks/create* "https://api.twitter.com/1.1/blocks/create.json")
(defvar *blocks/destroy* "https://api.twitter.com/1.1/blocks/destroy.json")
(defvar *users/lookup* "https://api.twitter.com/1.1/users/lookup.json")
(defvar *users/show* "https://api.twitter.com/1.1/users/show.json")
(defvar *users/search* "https://api.twitter.com/1.1/users/search.json")
(defvar *users/contributees* "https://api.twitter.com/1.1/users/contributees.json")
(defvar *users/contributors* "https://api.twitter.com/1.1/users/contributors.json")
(defvar *users/profile-banner* "https://api.twitter.com/1.1/users/profile_banner.json")

(defclass* user ()
  (id screen-name contributors created-at
   counts language location notifications status
   follow-request-sent following entities
   geo translator protected verified
   time-zone url utc-offset
   default name description show-inline-media
   background avatar colors)
  (:documentation "Class representation of a user."))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a #~d" (screen-name user) (id user)))
  user)

(define-make-* (user parameters)
  :id :screen-name :contributors :location :notifications 
  :follow-request-sent :following :protected :verified 
  :time-zone :utc-offset :url :name :location :description 
  :show-inline-media (:language . :lang) (:geo . :geo-enabled)
  (:translator . :is-translator) (:default . :default-profile)
  (:status (when-let ((param (cdr (assoc :status parameters)))) (make-status param)))
  (:entities (when-let ((param (cdr (assoc :entities parameters)))) (make-entities param)))
  (:created-at (parse-twitter-time (cdr (assoc :created-at parameters))))
  (:counts (list :statuses (cdr (assoc :statuses-count parameters))
                 :listed (cdr (assoc :listed-count parameters))
                 :friends (cdr (assoc :friends-count parameters))
                 :followers (cdr (assoc :followers-count parameters))
                 :favorites (cdr (assoc :favourites-count parameters))))
  (:background (list
                :color (cdr (assoc :profile-background-color parameters))
                :use-image (cdr (assoc :profile-use-background-image parameters))
                :image-url (cdr (assoc :profile-background-image-url parameters))
                :image-url-https (cdr (assoc :profile-background-image-url-https parameters))
                :image-tile (cdr (assoc :profile-background-image-tile parameters))))
  (:avatar (list
            :default (cdr (assoc :default-profile-image parameters))
            :image-url (cdr (assoc :profile-image-url parameters))
            :image-url-https (cdr (assoc :profile-image-url-https parameters))))
  (:colors (list
            :link (cdr (assoc :profile-link-color parameters))
            :text (cdr (assoc :profile-text-color parameters))
            :sidebar-border (cdr (assoc :profile-sidebar-border-color parameters))
            :sidebar-fill (cdr (assoc :profile-sidebar-fill-color parameters))))))

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
  (setf include-entities (when include-entities "true"))
  (make-user (signed-request *users/show* :parameters (prepare* screen-name user-id include-entities) :method :GET)))

(defun users/search (query &key (page 1) (count 5) include-entities)
  "Provides a simple, relevance-based search interface to public user accounts on Twitter. Try querying by topical interest, full name, company name, location, or other criteria. Exact match searches are not supported.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/search"
  (assert (< (* page count) 1000) () "Only the first 1000 results are available.")
  (setf include-entities (when include-entities "true"))
  (mapcar #'make-user (signed-request *users/search* :parameters (prepare* (q . query) page count include-entities) :method :GET)))

(defun users/contributees (&key user-id screen-name include-entities (skip-status T))
  "Returns a list of users that the specified user can \"contribute\" to.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/contributees"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (mapcar #'make-user (signed-request *users/contributees* :parameters (prepare* user-id screen-name include-entities skip-status) :method :GET)))

(defun users/contributors (&key user-id screen-name include-entities (skip-status T))
  "Returns a list of users who can contribute to the specified account.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/contributors"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (setf include-entities (when include-entities "true"))
  (setf skip-status (when skip-status "true"))
  (mapcar #'make-user (signed-request *users/contributors* :parameters (prepare* user-id screen-name include-entities skip-status) :method :GET)))

(defun users/profile-banner (&key user-id screen-name)
  "Returns a map of the available size variations of the specified user's profile banner. If the user has not uploaded a profile banner, a HTTP 404 will be served instead. This method can be used instead of string manipulation on the profile_banner_url returned in user objects as described in User Profile Images and Banners.

According to spec https://dev.twitter.com/docs/api/1.1/get/users/profile_banner"
  (assert (or screen-name user-id) () "Either SCREEN-NAME or USER-ID are required.")
  (when user-id (assert (numberp user-id) () "USER-ID must be a number."))
  (mapc #'(lambda (size) (setf (cdr size) (make-banner size)))
        (cdr (assoc :sizes (signed-request *users/profile-banner* :parameters (prepare* user-id screen-name) :method :GET)))))

(defgeneric save (object)
  (:documentation "Save the given object to twitter. 
Does not guarantee to save every attribute, see the individual functions."))

(defmethod save ((settings settings))
  "Save the account settings. Returns a new settings object as per ACCOUNT/SETTINGS/POST."
  (account/settings/post (when (trend settings) (woeid (trend settings)))
                         (sleep-time settings) (sleep-time-start settings) (sleep-time-end settings)
                         (time-zone settings) (language settings)))
