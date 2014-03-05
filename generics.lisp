#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defgeneric block! (user)
  (:documentation "Blocks the given user as per BLOCKS/CREATE. Returns a new user object.")
  (:method ((user user))
    (blocks/create :user-id (id user))))

(defgeneric unblock! (user)
  (:documentation "Unblocks the given user as per BLOCKS/DESTROY. Returns a new user object.")
  (:method ((user user))
    (blocks/destroy :user-id (id user))))

(defgeneric follow! (user)
  (:documentation "Follows the given user as per FRIENDSHIPS/CREATE. Returns a new user object.")
  (:method ((user user))
    (friendships/create :user-id (id user))))

(defgeneric unfollow! (user)
  (:documentation "Unfollows the given user as per FRIENDSHIPS/DESTROY. Returns a new user object.")
  (:method ((user user))
    (friendships/destroy :user-id (id user))))

(defgeneric report! (user)
  (:documentation "Reports the given user for spam and blocks it as per USERS/REPORT-SPAM. Returns a new user object.")
  (:method ((user user))
    (users/report-spam :user-id (id user))))

(defgeneric message! (user text)
  (:documentation "Sends a direct message to the given user as per DIRECT-MESSAGES/NEW. Returns the new DIRECT-MESSAGE Object.")
  (:method ((user user) text)
    (direct-messages/new text :user-id (id user))))

(defgeneric tweet! (text &key file reply-to latitude longitude place-id possibly-sensitive)
  (:documentation "Creates a new status as per STATUSES/UPDATE or STATUSES/UPDATE-WITH-MEDIA if FILE is given. Returns the new STATUS object.")
  (:method (text &key file reply-to latitude longitude place-id possibly-sensitive)
    (if file
        (statuses/update-with-media text file :possibly-sensitive possibly-sensitive :reply-to reply-to :latitude latitude :longitude longitude :place-id place-id)
        (statuses/update text :reply-to reply-to :latitude latitude :longitude longitude :place-id place-id))))

(defgeneric mention! (user text &key file reply-to latitude longitude place-id possibly-sensitive)
  (:documentation "Creates a new mentioning status (@user ..) as per TWEET!. Returns the new STATUS object.")
  (:method ((user user) text &key file reply-to latitude longitude place-id possibly-sensitive)
    (tweet! (format NIL "@~a ~a" (screen-name user) text)
            :file file :reply-to reply-to :latitude latitude :longitude longitude :place-id place-id :possibly-sensitive possibly-sensitive)))

(defgeneric delete! (object)
  (:documentation "Deletes the given object.")
  (:method ((status status))
    "Deletes the given STATUS as per STATUSES/DESTROY. Returns a new STATUS object."
    (statuses/destroy (id status)))
  (:method ((message direct-message))
    "Deletes the given DIRECT-MESSAGE as per DIRECT-MESSAGES/DESTROY. Returns a new DIRECT-MESSAGE object."
    (direct-messages/destroy (id message)))
  (:method ((list user-list))
    "Deletes the given USER-LIST as per LISTS/DESTROY. Returns a new USER-LIST object."
    (lists/destroy :list-id (id list)))
  (:method ((search saved-search))
    "Deltes the given SAVED-SEARCH as per SAVED-SEARCHES/DESTROY/ID. Returns a new SAVED-SEARCH object."
    (saved-searches/destroy/id (id search))))

(defgeneric reply! (status text &key file latitude longitude place-id possibly-sensitive)
  (:documentation "Replies to the given status, mentioning only the status' owner (@user ..) as per TWEET!. Returns the new STATUS object.")
  (:method ((status status) text &key file latitude longitude place-id possibly-sensitive)
    (mention! (user status) text :reply-to (id status) :file file :latitude latitude :longitude longitude :place-id place-id :possibly-sensitive possibly-sensitive)))

(defgeneric reply-all! (status text &key file latitude longitude place-id possibly-sensitive)
  (:documentation "Replies to all mentioned users in the tweet as per TWEET!. Returns the new STATUS object.")
  (:method ((status status) text &key file latitude longitude place-id possibly-sensitive)
    (tweet! (format NIL "~{@~a~^ ~} ~a" (mapcar #'screen-name (cdr (assoc :user-mentions (entities status)))) text)
            :file file :latitude latitude :longitude longitude :place-id place-id :possibly-sensitive possibly-sensitive)))

(defgeneric retweet! (status)
  (:documentation "Retweets the given status as per STATUSES/RETWEET. Returns the new STATUS object.")
  (:method ((status status))
    (statuses/retweet (id status))))

(defgeneric favorite! (status)
  (:documentation "Favorites the given status as per FAVORITES/CREATE. Returns a new STATUS object.")
  (:method ((status status))
    (favorites/create (id status))))

(defgeneric unfavorite! (status)
  (:documentation "Unfavorites the given status as per FAVORITES/DESTROY. Returns a new STATUS object.")
  (:method ((status status))
    (favorites/destroy (id status))))

(defgeneric list! (list user)
  (:documentation "Adds the given user to the given list as per LISTS/MEMBERS/CREATE. Returns the given USER.")
  (:method ((list user-list) (user user))
    (lists/members/create :user-id (id user) :list-id (id list))
    user))

(defgeneric unlist! (list user)
  (:documentation "Removes the given user from the given list as per LISTS/MEMBERS/DESTROY-ALL. Returns the given USER.")
  (:method ((list user-list) (user user))
    (lists/members/destroy-all :user-ids (list (id user)) :list-id (id list))
    user))

(defgeneric subscribe! (list)
  (:documentation "Subscribes to the given list as per LISTS/SUBSCRIBERS/CREATE. Returns a new USER-LIST object.")
  (:method ((list user-list))
    (lists/subscribers/create :list-id (id list))))

(defgeneric unsubscribe! (list)
  (:documentation "Unsubscribes from the given list as per LISTS/SUBSCRIBERS/DESTROY. REturns a new USER-LIST object.")
  (:method ((list user-list))
    (lists/subscribers/destroy :list-id (id list))))

(defgeneric stream! (filter handler-function &rest args &key stall-warnings filter-level language count &allow-other-keys)
  (:documentation "Starts the streaming process as per the STREAM/ functions. Depending on the filter, a different stream is started.
FILTER can be of type USER, LOCATION, GEOMETRY, STRING, NULL or :USER, :SITE, :SAMPLE, :FILTER or :FIREHOSE.")
  (:method ((user user) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream/statuses/filter handler-function :follow (list (id user)) args))
  
  (:method ((location location) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream! (bounding-box location) handler-function args))
  
  (:method ((geometry geometry) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream/statuses/filter handler-function :locations (apply #'concatenate 'list (coordinates geometry)) args))
  
  (:method ((string string) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream/statuses/filter handler-function :track (list string) args))
  
  (:method ((null null) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream/statuses/sample handler-function args))
  
  (:method ((user (eql :user)) handler-function &rest args &key stall-warnings (filter-level :none) language (with :user) replies count)
    (declare (ignore stall-warnings filter-level language count with replies))
    (apply #'stream/user handler-function args))
  
  (:method ((site (eql :site)) handler-function &rest args &key stall-warnings (filter-level :none) language (with :follow) replies count)
    (declare (ignore stall-warnings filter-level language count with replies))
    (apply #'stream/site handler-function args))

  (:method ((sample (eql :sample)) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream! NIL handler-function args))

  (:method ((sample (eql :filter)) handler-function &rest args &key follow track locations stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count follow track locations))
    (apply #'stream/statuses/filter handler-function args))
  
  (:method ((fire (eql :firehose)) handler-function &rest args &key stall-warnings (filter-level :none) language count)
    (declare (ignore stall-warnings filter-level language count))
    (apply #'stream/statuses/firehose handler-function args)))

(defgeneric fetch-user! (object)
  (:documentation "Fetches the user object associated with the given object. 
This always returns a fresh object and always results in a server call.")
  (:method ((user-id integer))
    "Fetches the user associated with the given ID."
    (users/show :user-id user-id))
  (:method ((screen-name string))
    "Fetches the user associated with the given screen name."
    (users/show :screen-name screen-name))
  (:method ((list list))
    "Fetches all users in the list. A list of IDs or screen names will be fetched through USERS/LOOKUP and is thus faster."
    (typecase (first list)
      (integer (users/lookup :user-ids list))
      (string (users/lookup :screen-names list))
      (null NIL)
      (T (mapcar #'fetch-user! list))))
  (:method ((null null))
    "Fetches the authenticated user object (ACCOUNT/SELF)."
    (account/self))
  (:method ((user user))
    "Fetches the complete user object."
    (fetch-user! (or (id user) (screen-name user))))
  (:method ((status status))
    "Fetches the user associated with the status."
    (fetch-user! (user status)))
  (:method ((message direct-message))
    "Fetches the user associated with the direct-message."
    (fetch-user! (sender message)))
  (:method ((relat relationship))
    "Fetches the user associated with the relationship."
    (fetch-user! (or (id relat) (screen-name relat))))
  (:method ((list user-list))
    "Fetches the owner of the user-list."
    (fetch-user! (user list)))
  (:method ((slug slug))
    "Fetches all suggested users in the slug."
    (fetch-user! (users slug))))
