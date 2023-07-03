(in-package #:org.tymoonnext.chirp)

(defvar *trends/place* "https://api.twitter.com/1.1/trends/place.json")
(defvar *trends/available* "https://api.twitter.com/1.1/trends/available.json")
(defvar *trends/closest* "https://api.twitter.com/1.1/trends/closest.json")

(defclass* trend ()
  (events name promoted-content query url)
  (:documentation "Class representation of a twitter trend object."))

(defmethod print-object ((trend trend) stream)
  (print-unreadable-object (trend stream :type T)
    (format stream "~a" (name trend)))
  trend)

(define-make-* (trend)
  :events :name  :promoted-content :query :url)

(defclass* trend-location ()
  (country country-code name parent place-code place-name url woeid)
  (:documentation "Class representation of a twitter trend location object."))

(defmethod print-object ((trend-location trend-location) stream)
  (print-unreadable-object (trend-location stream :type T)
    (format stream "~a #~d" (name trend-location) (woeid trend-location)))
  trend-location)

(define-make-* (trend-location parameters)
  :country :name :url :woeid
  (:country-code . :countrycode)
  (:parent . :parentid)
  (:place-code (cdr (assoc :code (cdr (assoc :placetype parameters)))))
  (:place-name (cdr (assoc :name (cdr (assoc :placetype parameters))))))

(defun trends/place (woeid &key exclude-hashtags)
  "Returns the top 10 trending topics for a specific WOEID, if trending information is available for it.

According to spec https://api.twitter.com/1.1/trends/place.json"
  (let ((data (signed-request *trends/place* :parameters (prepare
                                                          `(("id" . ,woeid)
                                                            ("exclude" . ,(when exclude-hashtags "hashtags")))) :method :GET)))
    (mapcar #'make-trend
            (cdr (assoc :trends (first data))))))

(defun trends/available ()
  "Returns the locations that Twitter has trending topic information for.

According to spec https://api.twitter.com/1.1/trends/available.json"
  (let ((data (signed-request *trends/available* :method :GET)))
    (mapcar #'make-trend-location data)))

(defun trends/closest (latitude longitude)
  "Returns the locations that Twitter has trending topic information for, closest to a specified location.

According to spec https://dev.twitter.com/docs/api/1.1/get/trends/closest"
  (let ((data (signed-request *trends/closest* :parameters `(("lat" . ,latitude)
                                                             ("long" . ,longitude)) :method :GET)))
    (mapcar #'make-trend-location data)))

(defgeneric closest-trend-locations (location)
  (:documentation "Performs a TRENDS/CLOSEST request on a location object."))

(defmethod closest-trend-locations ((location location))
  (trends/closest (latitude location) (longitude location)))

(defgeneric closest-trends (location &key exclude-hashtags)
  (:documentation "Performs a TRENDS/PLACE request on a location object."))

(defmethod closest-trends ((location location) &key exclude-hashtags)
  (loop with trends = ()
        for location in (closest-trend-locations location)
        do (setf trends (append trends (trends/place (woeid location) :exclude-hashtags exclude-hashtags)))))

(defmethod closest-trends ((location trend-location) &key exclude-hashtags)
  (trends/place (woeid location) :exclude-hashtags exclude-hashtags))
