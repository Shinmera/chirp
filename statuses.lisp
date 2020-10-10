#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/retweets* "https://api.twitter.com/1.1/statuses/retweets/~a.json")
(defvar *statuses/show* "https://api.twitter.com/1.1/statuses/show/~a.json")
(defvar *statuses/destroy* "https://api.twitter.com/1.1/statuses/destroy/~a.json")
(defvar *statuses/retweet* "https://api.twitter.com/1.1/statuses/retweet/~a.json")
(defvar *statuses/update* "https://api.twitter.com/1.1/statuses/update.json")
(defvar *statuses/update-with-media* "https://api.twitter.com/1.1/statuses/update_with_media.json")
(defvar *statuses/oembed* "https://api.twitter.com/1.1/statuses/oembed.json")
(defvar *statuses/retweeters/ids* "https://api.twitter.com/1.1/statuses/retweeters/ids.json")

(defclass* status ()
  (id text full-text display-text-range entities extended-entities created-at
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
    (when (user status) (format stream "~a " (screen-name (user status))))
    (format stream "#~d" (id status))
    (when (in-reply-to status) (format stream " @~a" (cdr (assoc :screen-name (in-reply-to status)))))
    (when (retweeted-status status) (format stream " RT~a" (retweeted-status status))))
  status)

(define-make-* (status parameters)
  :id :text :full-text :display-text-range :source :filter-level :scopes
  :possibly-sensitive :retweeted :favorited :truncated
  :withheld-copyright :withheld-in-countries :withheld-scope
  (:counts `((:favorites . ,(cdr (assoc :favorite-count parameters)))
             (:retweets . ,(cdr (assoc :retweet-count parameters)))))
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
  (:extended-entities (parse-when-param :extended-entities #'make-entities))
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

(defun statuses/retweets (id &key (count 100) trim-user tweet-mode)
  "Returns a collection of the 100 most recent retweets of the tweet specified by the id parameter.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/retweets/%3Aid"
  (assert (<= count 100) () "Count must be less than or equal to 100.")
  (mapcar #'make-status (signed-request (format NIL *statuses/retweets* id) :parameters (prepare* count trim-user tweet-mode) :method :GET)))

(defun statuses/show (id &key trim-user include-my-retweet (include-entities T) tweet-mode)
  "Returns a single Tweet, specified by the id parameter. The Tweet's author will also be embedded within the tweet.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/show/%3Aid"
  (unless include-entities (setf include-entities "false"))
  (make-status (signed-request (format NIL *statuses/show* id) :parameters (prepare* trim-user include-my-retweet include-entities tweet-mode) :method :GET)))

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

;; Fixme
(defparameter *http-url-regex* (cl-ppcre:create-scanner "http://[\\w\\d\\-.]+\\.\\w{2,}(/[\\w\\d\\-%+?=&@#.;/]*)?"))
(defparameter *https-url-regex* (cl-ppcre:create-scanner "https://[\\w\\d\\-.]+\\.\\w{2,}(/[\\w\\d\\-%+?=&@#.;/]*)?"))
(defun compute-status-length (status-text)
  "Computes the final status length by shortening the URLs within the tweet according to twitter's current URL shortening configurations.
If the configuration has not been fetched before, this function call will result in an api call."
  (let* ((config (help/configuration))
         (http-url (make-string (short-url-length config) :initial-element #\X))
         (https-url (make-string (short-url-length-https config) :initial-element #\X)))
    (length
     (cl-ppcre:regex-replace-all
      *http-url-regex*
      (cl-ppcre:regex-replace-all *https-url-regex* status-text https-url)
      http-url))))

(define-indentation 'replace-entity '(4 4 2 2))
(defun replace-entity (status entity-type replacement-function &optional (text (text status)))
  "Replaces the regions as marked by the entities with the result of the replacement function.

STATUS               --- A status object or any object with entities.
ENTITY-TYPE          --- A keyword for the entity to replace. Either :USER-MENTIONS :URLS :SYMBOLS :HASHTAGS :MEDIA
REPLACEMENT-FUNCTION --- A function with one argument; the entity object currently being replaced.
TEXT                 --- The text to replace in. The sequence is not modified."
  (let ((result (make-string-output-stream)))
    (loop with last = 0
          for entity in (cdr (assoc entity-type (entities status)))
          do (write-string (subseq text last (start entity)) result)
             (write-string (funcall replacement-function entity) result)
             (setf last (end entity))
          finally (write-string (subseq text last) result))
    (get-output-stream-string result)))

(defun replace-entities (status replacement-functions &optional (text (text status)))
  "Replaces the regions as marked by the entities with the result of the replacement function.

STATUS                --- A status object or any object with entities, or a list of entities.
REPLACEMENT-FUNCTIONS --- A plist of entity-types as keys and functions with one argument as the value. Keys should
                          be one of :USER-MENTIONS :URLS :SYMBOLS :HASHTAGS :MEDIA. Functions should take one argument,
                          the entity to replace and return a string value to replace it with.
TEXT                  --- The text to replace in. The sequence is not modified."
  (let ((entities (remove-duplicates (sort (flatten-sublists (if (listp status) status (entities status)))
                                           #'< :key (lambda (ent) (start (cdr ent))))
                                     :key (lambda (ent) (start (cdr ent))))))
    (with-output-to-string (result)
      (loop with last = 0
            for (type . entity) in entities
            do (when-let ((func (getf replacement-functions type)))
                 (write-string text result :start last :end (start entity))
                 (write-string (funcall func entity) result)
                 (setf last (end entity)))
            finally (write-string (subseq text last) result)))))

(defmacro with-replaced-entities ((status &optional (entityvar 'entity) (text `(text ,status))) &body replacements)
  "Shorthand macro for replacing multiple entities.

STATUS       --- A status object or any object with entities, or a list of entities.
ENTITYVAR    --- The variable to use in the replacement functions.
TEXT         --- The text to pass to REPLACE-ENTITIES
REPLACEMENTS ::= (TYPE FORM*)*
TYPE         --- An entity type. Should be one of :USER-MENTIONS :URLS :SYMBOLS :HASHTAGS :MEDIA."
  `(replace-entities
    ,status
    (list ,@(loop for (type . body) in replacements
                  append (list type `#'(lambda (,entityvar) ,@body))))
    ,text))

(defun text-with-expanded-urls (status &optional (text (text status)))
  "Replaces the shortened links with the resolved entity urls if possible."
  (replace-entities status (list :media #'expanded-url
                                 :urls #'expanded-url)
                    text))

(defun text-with-markup (status &key append-media (url-class "status-url") (mention-class "status-mention")
                                     (hashtag-class "status-hashtag") (media-class "status-media") (media-image-class "status-media-image")
                                     (text (text status)))
  "Transforms the text into a HTML-ready form.
This includes the following changes related to entities:
  URLS          -> <a href=\"URL\" title=\"EXPANDED-URL\">DISPLAY-URL</a>
  USER-MENTIONS -> <a href=\"http://twitter.com/SCREEN-NAME\" title=\"NAME\">@SCREEN-NAME</a>
  HASHTAGS      -> <a href=\"http://twitter.com/search?q=%23HASHTAG\">#HASHTAG</a>
  MEDIA         -> <a href=\"URL\" title=\"EXPANDED-URL\">DISPLAY-URL</a>
The tweet status text is also correctly escaped with entities for <, >, &.
If APPEND-MEDIA is non-NIL, an <img> tag with the related SRC and ALT attributes is appended to the text if a media entity exists.
APPEND-MEDIA can be one of :LARGE :MEDIUM :SMALL :THUMB, which sets the proper width and height attributes and loads the respective image.
The size defaults to :THUMB."
  (macrolet ((ent-func (entityvar string &rest vars)
               `#'(lambda (,entityvar) (format NIL ,string ,@vars))))
    (let ((text
            (with-replaced-entities (status entity text)
              (:urls
               (format NIL "<a href=\"~a\" title=\"~a\" class=\"~a\">~a</a>"
                       (url entity) (expanded-url entity) url-class (display-url entity)))
              (:user-mentions
               (format NIL "<a href=\"http://twitter.com/~a\" title=\"~a\" class=\"~a\">~a</a>"
                         (screen-name entity) (name entity) mention-class (screen-name entity)))
              (:hashtags
               (format NIL "<a href=\"http://twitter.com/search?q=~a\" class=\"~a\">~a</a>"
                         (text entity) hashtag-class (text entity)))
              (:media
               (format NIL "<a href=\"~a\" title=\"~a\" class=\"~a\">~a</a>"
                         (url entity) (expanded-url entity) media-class (display-url entity))))))
      (when append-media
        (unless (member append-media '(:LARGE :MEDIUM :SMALL :THUMB))
          (setf append-media :THUMB))
        (dolist (entity (cdr (assoc :media (if (listp status) status (entities status)))))
          (let ((size (cdr (assoc append-media (sizes entity)))))
            (setf text (format NIL "~a <a href=\"~a\" title=\"~a\" class=\"~a\"><img src=\"~a:~a\" alt=\"~a\" width=\"~a\" height=\"~a\" /></a>"
                               text
                               (url entity) (expanded-url entity) media-image-class
                               (media-url-https entity) (string-downcase append-media) (media-type entity)
                               (width size) (height size))))))
      text)))

(defun retweet-p (status)
  "Returns T if the status is a retweet.
To qualify as a retweet, the tweet has to either contain a RETWEETED-STATUS, or start with \"RT\"."
  (not (null (or (retweeted-status status)
                 (and (< 2 (length (text status)))
                      (string= "RT" (text status) :end2 2))))))

(defun direct-mention-p (status)
  "Returns T if the status is a direct mention of the currently identified user.
To qualify as a direct mention, the mention has to appear before any other text in the tweet."
  ;; Fixme
  (when (entities status)
    (when-let ((mentions (cdr (assoc :user-mentions (entities status)))))
      (string-equal (screen-name (account/self)) (screen-name (first mentions))))))

(defun mention-p (status)
  "Returns T if the status is mentioning the currently identified user.
Simply checks the status entities for a user-mention that matches to the currently identified user."
  (when (entities status)
    (when-let ((mentions (cdr (assoc :user-mentions (entities status)))))
      (member (screen-name (account/self)) mentions
              :key #'screen-name :test #'string-equal))))
