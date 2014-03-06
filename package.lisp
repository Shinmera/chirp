#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.chirp
  (:nicknames #:chirp)
  (:use #:cl #:split-sequence #:alexandria)
  (:shadow #:parse-body)
  ;; account.lisp
  (:export
   #:settings
   #:force-https
   #:email-discoverable
   #:geo
   #:language
   #:protected
   #:screen-name
   #:show-inline-media
   #:sleep-time
   #:sleep-time-start
   #:sleep-time-end
   #:time-zone-name
   #:time-zone-info
   #:time-zone-offset
   #:cookie-personalization
   #:trend
   
   #:account/settings
   #:account/verify-credentials
   #:account/self
   #:account/update-profile
   #:account/update-profile-background-image
   #:account/update-profile-colors
   #:account/update-profile-image
   #:account/update-profile-banner
   #:account/remove-profile-banner)
  ;; blocks.lisp
  (:export
   #:blocks/list
   #:blocks/ids
   #:blocks/create
   #:blocks/destroy)
  ;; cursor.lisp
  (:export
   #:cursor
   #:id
   #:url
   #:parameters
   #:data
   #:request-method
   #:cursored-request
   #:cursor-request
   #:cursor-next
   #:cursor-previous
   #:do-cursor
   #:map-cursor
   #:cursor-collect)
  ;; directmessages.lisp
  (:export
   #:direct-message
   #:id
   #:text
   #:recipient
   #:sender
   #:created-at
   #:entities
   
   #:direct-messages
   #:direct-messages/sent
   #:direct-messages/show
   #:direct-messages/destroy
   #:direct-messages/new)
  ;; entities.lisp
  (:export
   #:entity
   
   #:hashtag
   #:text
   #:start
   #:end
   
   #:media
   #:url
   #:id
   #:display-url
   #:expanded-url
   #:start
   #:end
   #:sizes
   #:media-url
   #:media-url-https
   #:media-type
   #:source-status
   
   #:size
   #:height
   #:width
   #:resize-method
   
   #:url
   #:display-url
   #:expanded-url
   #:start
   #:end
   
   #:mention
   #:id
   #:start
   #:end
   #:name
   #:screen-name
   
   #:t-symbol
   #:text
   #:start
   #:end)
  ;; favorites.lisp
  (:export
   #:favorites/list
   #:favorites/destroy
   #:favorites/create)
  ;; friends.lisp
  (:export
   #:*connection-values
   
   #:relationship
   #:id
   #:screen-name
   #:name
   #:followed-by
   #:following
   #:can-dm
   #:blocking
   #:all-replies
   #:want-retweets
   #:marked-spam
   #:notifications-enabled
   #:connections
   
   #:friends/ids
   #:friends/list
   
   #:followers/ids
   #:followers/list
   
   #:friendships/incoming
   #:friendships/outgoing
   #:friendships/create
   #:friendships/destroy
   #:friendships/update
   #:friendships/show
   #:friendships/lookup
   #:friendships/no-retweets/ids)
  ;; generics.lisp
  (:export
   #:block!
   #:unblock!
   #:follow!
   #:unfollow!
   #:report!
   #:message!
   #:tweet!
   #:mention!
   #:delete!
   #:reply!
   #:reply-all!
   #:retweet!
   #:favorite!
   #:unfavorite!
   #:list!
   #:unlist!
   #:subscribe!
   #:unsubscribe!
   #:stream!
   #:fetch-user!
   #:map-timeline!)
  ;; help.lisp
  (:export
   #:*cached-languages*
   #:*cached-configuration*
   
   #:configuration
   #:photo-size-limit
   #:photo-sizes
   #:short-url-length
   #:short-url-length-https
   #:non-username-paths
   #:max-media-per-upload
   #:characters-reserved-per-media
   
   #:language
   #:name
   #:code
   #:status
   
   #:resource
   #:address
   #:remaining
   #:reset
   #:limit
   
   #:help/configuration
   #:help/languages
   #:help/privacy
   #:help/tos
   #:application/rate-limit-status
   #:valid-language-p)
  ;; lists.lisp
  (:export
   #:user-list
   #:id
   #:user
   #:name
   #:full-name
   #:description
   #:created-at
   #:uri
   #:counts
   #:mode
   #:following
   #:slug
   
   #:lists/list
   #:lists/statuses
   #:lists/show
   #:lists/create
   #:lists/update
   #:lists/destroy
   #:lists/members
   #:lists/members/show
   #:lists/members/create
   #:lists/members/create-all
   #:lists/members/destroy
   #:lists/members/destroy-all
   #:lists/subscribers
   #:lists/subscribers/show
   #:lists/subscribers/create
   #:lists/subscribers/destroy
   #:lists/subscriptions
   #:lists/memberships
   #:lists/ownerships)
  ;; location.lisp
  (:export
   #:location
   #:id
   #:name
   #:full-name
   #:location-type
   #:latitude
   #:longitude
   #:url
   #:country-name
   #:country-code
   #:bounding-box
   #:poly-lines
   #:contained-within
   
   #:geometry
   #:shape
   #:coordinates
   
   #:geo/id
   #:geo/reverse-geocode
   #:geo/search
   #:geo/similar-places)
  ;; oauth.lisp
  (:export
   #:*oauth-api-key*
   #:*oauth-api-secret*
   #:*oauth-access-token*
   #:*oauth-access-secret*
   #:*oauth-signature-method*
   #:*oauth-version*
   #:*server-port*
   
   #:oauth-error
   
   #:oauth-parameter-missing
   #:parameter
   
   #:oauth-request-error
   #:http-status
   #:http-body
   #:http-headers
   #:target-url
   #:target-method
   #:target-parameters
   #:target-headers

   #:signed-request
   #:signed-data-request
   #:signed-stream-request
   #:oauth/request-token
   #:pin-request-token
   #:oauth/authenticate
   #:oauth/authorize
   #:oauth/access-token
   #:complete-authentication
   #:initiate-authentication

   #:access-level)
  ;; saved-searches.lisp
  (:export
   #:saved-search
   #:id
   #:created-at
   #:name
   #:search-position
   #:query
   
   #:saved-searches/list
   #:saved-searches/show/id
   #:saved-searches/create
   #:saved-searches/destroy/id)
  ;; search.lisp
  (:export
   #:search-metadata
   #:max-id
   #:since-id
   #:refresh-url
   #:next-results
   #:result-count
   #:completed-in
   #:query
   
   #:search/tweets)
  ;; statuses.lisp
  (:export
   #:status
   #:id
   #:text
   #:entities
   #:created-at
   #:user
   #:contributors
   #:source
   #:coordinates
   #:geo
   #:place
   #:retweeted-status
   #:counts
   #:in-reply-to
   #:possibly-sensitive
   #:retweeted
   #:favorited
   #:truncated
   
   #:oembed
   #:html
   #:url
   #:height
   #:width
   #:version
   #:oembed-type
   #:cache-age
   #:author-name
   #:author-url
   #:provider-url
   #:provider-name
   
   #:statuses/retweets
   #:statuses/show
   #:statuses/destroy
   #:statuses/retweet
   #:statuses/update
   #:statuses/update-with-media
   #:statuses/oembed
   #:statuses/retweeters/ids

   #:compute-status-length)
  ;; stream.lisp
  (:export
   #:message
   
   #:stream-unknown
   #:data
   
   #:stream-delete
   #:id
   #:user-id
   
   #:stream-scrub-geo
   #:up-to-status-id
   #:user-id
   
   #:stream-limit
   #:track
   
   #:stream-status-withheld
   #:id
   #:withheld-in-countries
   
   #:stream-user-withheld
   #:id
   #:withheld-in-countries
   
   #:stream-disconnect
   #:code
   #:stream-name
   #:reason
   
   #:stream-friends
   #:friends
   
   #:stream-event
   #:target
   #:source
   #:target-object
   #:created-at
   
   #:event-unknown
   #:event-access-revoked
   #:event-block
   #:event-unblock
   #:event-favorite
   #:event-unfavorite
   #:event-follow
   #:event-unfollow
   #:event-list-create
   #:event-list-destroy
   #:event-list-update
   #:event-list-member-add
   #:event-list-member-remove
   #:event-list-user-subscribe
   #:event-list-user-unsubscribe
   #:event-user-update
   
   #:stream-warning
   #:code
   #:message
   #:user-id
   
   #:stream-control
   #:control-uri
   
   #:stream-envelope
   #:for-user
   #:message
   
   #:stream/user
   #:stream/site
   #:stream/statuses/filter
   #:stream/statuses/sample
   #:stream/statuses/firehose)
  ;; suggestions.lisp
  (:export
   #:slug
   #:name
   #:size
   #:users
   
   #:users/suggestions/slug
   #:users/suggestions
   #:users/suggestions/slug/members)
  ;; timelines.lisp
  (:export
   #:statuses/mentions-timeline
   #:statuses/user-timeline
   #:statuses/home-timeline
   #:statuses/retweets-of-me)
  ;; toolkit.lisp
  (:export
   #:parse-month
   #:parse-twitter-time
   #:parse-boolean
   #:generate-nonce
   #:to-keyword
   #:from-keyword
   #:url-encode
   #:hmac
   #:prepare
   #:prepare*
   #:serialize-object
   #:file-to-base64-string)
  ;; trends.lisp
  (:export
   #:trend
   #:events
   #:name
   #:promoted-content
   #:query
   #:url
   
   #:trend-location
   #:country
   #:country-code
   #:name
   #:parent
   #:place-code
   #:place-name
   #:url
   #:woeid
   
   #:trends/place
   #:trends/available
   #:trends/closest)
  ;; user.lisp
  (:export
   #:user
   #:id
   #:screen-name
   #:contributors
   #:created-at
   #:coutns
   #:language
   #:location
   #:notifications
   #:status
   #:follow-request-sent
   #:following
   #:entities
   #:geo
   #:translator
   #:protected
   #:verified
   #:time-zone
   #:url
   #:utc-offset
   #:default
   #:name
   #:description
   #:show-inline-media
   #:background
   #:avatar
   #:colors
   
   #:banner
   #:size
   #:width
   #:height
   #:url
   
   #:users/lookup
   #:users/show
   #:users/search
   #:users/contributees
   #:users/contributors
   #:users/profile-banner
   #:users/report-spam)
  )
