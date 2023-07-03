(in-package #:org.tymoonnext.chirp)

(defvar *search/tweets* "https://api.twitter.com/1.1/search/tweets.json")

(defclass* search-metadata ()
  (max-id since-id refresh-url next-results result-count completed-in query)
  (:documentation "Class representation of the twitter search metadata object."))

(defmethod print-object ((meta search-metadata) stream)
  (print-unreadable-object (meta stream :type T)
    (format stream "~a" (query meta)))
  meta)

(define-make-* (search-metadata)
  :max-id :since-id :refresh-url :next-results (:result-count . :count) :completed-in :query)

(defun search/tweets (query &key latitude longitude radius locale language (result-type :mixed) (count 15) until since-id max-id (include-entities T))
  "Returns a collection of relevant Tweets matching a specified query and some search metadata as a second value.

According to spec https://dev.twitter.com/docs/api/1.1/get/search/tweets"
  (assert (or (and latitude longitude radius)
              (not (or latitude longitude radius))) () "LATITUDE, LONGITUDE and RADIUS must either all be specified, or none.")
  (when language (assert (valid-language-p language) () "~a is not a supported language." language))
  (assert (member result-type '(:mixed :recent :popular)) () "MEMBER must be one of :MIXED :RECENT :POPULAR")
  (assert (<= count 100) () "COUNT cannot be larger than 100.")
  (etypecase until
    (string) (null)
    (local-time:timestamp
     (setf until (local-time:format-timestring NIL until :format '((:year 4) #\- (:month 2) #\- (:day 2))))))
  (unless include-entities (setf include-entities "false"))
  (let* ((geocode (when latitude (format NIL "~a,~a,~a" latitude longitude radius)))
         (data (signed-request *search/tweets*
                               :parameters (prepare* (q . query) geocode locale language result-type count until since-id max-id include-entities)
                               :method :GET)))
    (values (mapcar #'make-status (cdr (assoc :statuses data)))
            (make-search-metadata (cdr (assoc :search-metadata data))))))
