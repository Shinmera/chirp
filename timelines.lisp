#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *statuses/mentions-timeline* "https://api.twitter.com/1.1/statuses/mentions_timeline.json")
(defvar *statuses/user-timeline* "https://api.twitter.com/1.1/statuses/user_timeline.json")
(defvar *statuses/home-timeline* "https://api.twitter.com/1.1/statuses/home_timeline.json")
(defvar *statuses/retweets-of-me* "https://api.twitter.com/1.1/statuses/retweets_of_me.json")

(defun statuses/mentions-timeline (&key (count 20) since-id max-id trim-user (include-entities T) contributor-details)
  "Returns the 20 most recent mentions (tweets containing a users's @screen_name) for the authenticating user.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/mentions_timeline"
  (assert (<= count 200) () "COUNT cannot be higher than 200.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-status (signed-request *statuses/mentions-timeline*
                                        :parameters (prepare* count since-id max-id trim-user include-entities contributor-details)
                                        :method :GET)))

(defun statuses/user-timeline (&key user-id screen-name (count 20) since-id max-id trim-user exclude-replies (include-entities T) contributor-details (include-rts T))
  "Returns a collection of the most recent Tweets posted by the user indicated by the screen_name or user_id parameters.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline"
  (assert (<= count 200) () "COUNT cannot be higher than 200.")
  (assert (or user-id screen-name) () "Either USER-ID or SCREEN-NAME are required.")
  (unless include-rts (setf include-rts "false"))
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-status (signed-request *statuses/user-timeline*
                                        :parameters (prepare* user-id screen-name count since-id max-id trim-user exclude-replies include-entities contributor-details include-rts)
                                        :method :GET)))

(defun statuses/home-timeline (&key (count 20) since-id max-id trim-user exclude-replies (include-entities T) contributor-details)
  "Returns a collection of the most recent Tweets and retweets posted by the authenticating user and the users they follow. The home timeline is central to how most users interact with the Twitter service.

According to spec https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline"
  (assert (<= count 200) () "Count cannot be higher than 200.")
  (unless include-entities (setf include-entities "false"))
  (mapcar #'make-status (signed-request *statuses/home-timeline*
                                        :parameters (prepare* count since-id max-id trim-user exclude-replies include-entities contributor-details)
                                        :method :GET)))

(defun statuses/retweets-of-me (&key (count 20) since-id max-id trim-user (include-entities T) (include-user-entities T))
  "Returns the most recent tweets authored by the authenticating user that have been retweeted by others. This timeline is a subset of the user's GET statuses/user_timeline.

According to spec "
  (assert (<= count 200) () "Count cannot be higher than 200.")
  (unless include-entities (setf include-entities "false"))
  (unless include-user-entities (setf include-user-entities "false"))
  (mapcar #'make-status (signed-request *statuses/retweets-of-me*
                                        :parameters (prepare* count since-id max-id trim-user include-entities include-user-entities)
                                        :method :GET)))
