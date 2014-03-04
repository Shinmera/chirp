#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/retweets* "https://api.twitter.com/1.1/statuses/retweets/~a.json")
(defvar *statuses/show* "https://api.twitter.com/1.1/statuses/show.json")
(defvar *statuses/destroy* "https://api.twitter.com/1.1/statuses/destroy/~a.json")
(defvar *statuses/retweet* "https://api.twitter.com/1.1/statuses/retweet/~a.json")
(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")
(defvar *statuses/update-with-media* "https://api.twitter.com/1.1/statuses/update_with_media.json")
(defvar *statuses/oembed* "https://api.twitter.com/1.1/statuses/oembed.json")
(defvar *statuses/retweeters/ids* "https://api.twitter.com/1.1/statuses/retweeters/ids.json")

(defclass* status ()
  (id text entities created-at
   user contributors source
   coordinates place
   retweeted-status filter-level scopes
   counts ;favorite-count retweet-count
   in-reply-to ;in-reply-to-status-id in-reply-to-user-id in-reply-to-screen-name
   possibly-sensitive retweeted favorited truncated
   withheld-copyright withheld-in-countries withheld-scope)
  (:documentation "Class representation of a twitter status (tweet).

According to spec https://dev.twitter.com/docs/platform-objects/tweets"))

(defmethod print-object ((status status) stream)
  (print-unreadable-object (status stream :type T)
    (format stream "~a #~d" (screen-name (user status)) (id status))
    (when (in-reply-to status) (format stream " @~a" (cdr (assoc :screen-name (in-reply-to status)))))
    (when (retweeted-status status) (format stream " RT~a" (retweeted-status status))))
  status)

(define-make-* (status parameters)
  :id :text :source :filter-level :scopes
  :possibly-sensitive :retweeted :favorited :truncated
  :withheld-copyright :withheld-in-countries :withheld-scope
  (:counts `((:favorites . ,(cdr (assoc :favorites parameters)))
             (:retweets . ,(cdr (assoc :retweets parameters)))))
  (:in-reply-to (when (cdr (assoc :in-reply-to-status-id parameters))
                  `((:id . ,(cdr (assoc :in-reply-to-status-id parameters)))
                    (:user-id . ,(cdr (assoc :in-reply-to-user-id parameters)))
                    (:screen-name . ,(cdr (assoc :in-reply-to-screen-name parameters))))))
  (:place (parse-when-param :place #'make-location))
  (:contributors (parse-when-param :contributors #'(lambda (list) (mapcar #'make-user list))))
  (:coordinates (parse-when-param :coordinates #'make-geometry))
  (:user (parse-when-param :user #'make-user))
  (:retweeted-status (parse-when-param :retweeted-status #'make-status))
  (:entities (parse-when-param :entities #'make-entities))
  (:created-at (parse-when-param :created-at #'parse-twitter-time)))

(defclass* oembed ()
  (html url height width version oembed-type cache-age
   author-name author-url
   provider-url provider-name)
  (:documentation "Class representation of a twitter oembed object."))

(define-make-* (oembed parameters)
  :html :url :width :height :version (:oembed-type . :type)
  :author-name :author-url :provider-url :provider-name
  (:cache-age (parse-when-param :cache-age #'local-time:unix-to-timestamp)))

(defun statuses/retweets (id &key (count 100) (trim-user T))
  "Returns a collection of the 100 most recent retweets of the tweet specified by the id parameter.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/retweets/%3Aid"
  (assert (<= count 100) () "Count must be less than or equal to 100.")
  (mapcar #'make-status (signed-request (format NIL *statuses/retweets* id) :parameters (prepare* count trim-user) :method :GET)))

(defun statuses/show (id &key (trim-user T) include-my-retweet (include-entities T))
  "Returns a single Tweet, specified by the id parameter. The Tweet's author will also be embedded within the tweet.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/show/%3Aid"
  (unless include-entities (setf include-entities "false"))
  (make-status (signed-request (format NIL *statuses/show* id) :parameters (prepare* trim-user include-my-retweet include-entities) :method :GET)))

(defun statuses/destroy (id &key trim-user)
  "Destroys the status specified by the required ID parameter. The authenticating user must be the author of the specified status. Returns the destroyed status if successful.

According to spec https://dev.twitter.com/docs/api/1.1/post/statuses/destroy/%3Aid"
  (make-status (signed-request (format NIL *statuses/destroy* id) :parameters (prepare* trim-user) :method :POST)))

(defun statuses/retweet (id &key trim-user)
  "Retweets a tweet. Returns the original tweet with retweet details embedded.

According to spec https://dev.twitter.com/docs/api/1.1/post/statuses/retweet/%3Aid"
  (make-status (signed-request (format NIL *statuses/retweet* id) :parameters (prepare* trim-user) :method :POST)))

(defun statuses/update (status &key reply-to latitude longitude place-id display-coordinates trim-user)
  "Updates the authenticating user's current status, also known as tweeting. To upload an image to accompany the tweet, use POST statuses/update_with_media.

According to spec https://dev.twitter.com/docs/api/1.1/post/statuses/update"
  (let ((parameters (prepare* status (in-reply-to-status-id . reply-to) (lat . latitude)
                              (long . longitude) place-id display-coordinates trim-user)))
    (make-status (signed-request *statuses/update* :parameters parameters :method :POST))))

(defun statuses/update-with-media (status media &key possibly-sensitive reply-to latitude longitude place-id display-coordinates trim-user)
  "Updates the authenticating user's current status and attaches media for upload. In other words, it creates a Tweet with a picture attached. MEDIA is either a pathname, usb-8 array or a base64-encoded string, or a list thereof.

According to spec https://dev.twitter.com/docs/api/1.1/post/statuses/update_with_media"
  (setf media (mapcar #'(lambda (medium) `("media[]". ,medium))
                      (if (listp media) media (list media))))
  (let ((parameters (prepare* status possibly-sensitive (in-reply-to-status-id . reply-to)
                              (lat . latitude) (long . longitude) place-id display-coordinates trim-user)))
    (make-status (signed-data-request *statuses/update-with-media* :data-parameters media :parameters parameters :method :POST))))

(defun statuses/oembed (id url &key max-width hide-media hide-thread omit-script (align :none) related language)
  "Returns information allowing the creation of an embedded representation of a Tweet on third party sites. See the oEmbed specification for information about the response format.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/oembed"
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (assert (and (<= 250 max-width) (<= max-width 550)) () "MAX-WIDTH must be between 250 and 550.")
  (assert (member align '(:left :right :center :none)) () "ALIGN must be one of :LEFT :RIGHT :CENTER :NONE")
  (make-oembed (signed-request *statuses/oembed* :parameters (prepare* id url max-width hide-media hide-thread omit-script align related (lang . language)) :method :GET)))

(defun statuses/retweeters/ids (id)
  "Returns a collection of up to 100 user IDs belonging to users who have retweeted the tweet specified by the id parameter.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/retweeters/ids"
  (cursor-collect :ids *statuses/retweeters/ids* :parameters (prepare* id)))
