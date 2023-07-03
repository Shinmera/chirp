(in-package #:org.tymoonnext.chirp)

(defvar *geo/id* "https://api.twitter.com/1.1/geo/id/~a.json")
(defvar *geo/reverse-geocode* "https://api.twitter.com/1.1/geo/reverse_geocode.json")
(defvar *geo/search* "https://api.twitter.com/1.1/geo/search.json")
(defvar *geo/similar-places* "https://api.twitter.com/1.1/geo/similar_places.json")

(defclass* location ()
  (id name full-name location-type latitude longitude url attributes
   country-name country-code bounding-box poly-lines contained-within)
  (:documentation "Twitter object containing Locational data. Used for the Places & Geo API.

According to spec https://dev.twitter.com/docs/platform-objects/places"))

(defmethod print-object ((location location) stream)
  (print-unreadable-object (location stream :type T)
    (format stream "~a #~a" (name location)  (id location)))
  location)

(define-make-* (location parameters)
  :id :name :full-name :url :poly-lines :attributes
  (:location-type . :place-type)
  (:latitude . :lat) (:longitude . :long)
  (:country-name . :country) :country-code
  (:bounding-box (parse-when-param :bounding-box #'make-geometry))
  (:contained-within (parse-when-param :contained-wihtin #'make-location)))

(defclass* geometry ()
  (shape coordinates)
  (:default-initargs :shape (error "Shape type required") :coordinates NIL)
  (:documentation "Object for locations containing geometrical shape data."))

(defmethod print-object ((geometry geometry) stream)
  (print-unreadable-object (geometry stream :type T :identity T)
    (format stream "~a" (shape geometry))))

(define-make-* (geometry)
  (:shape . :type) :coordinates)

(defun geo/id (place-id)
  "Returns a location object containing all the information about a known place.

According to spec https://dev.twitter.com/docs/api/1.1/param/geo/id/%3Aplace_id"
  (make-location (signed-request (format NIL *geo/id* place-id) :method :GET)))

(defun geo/reverse-geocode (latitude longitude &key accuracy granularity max-results)
  "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status.

According to spec https://dev.twitter.com/docs/api/1.1/get/geo/reverse_geocode"
  (when granularity (ecase granularity (:POI) (:NEIGHBORHOOD) (:CITY) (:ADMIN) (:COUNTRY)))
  (assert (numberp latitude) () "latitude must be a number.")
  (assert (numberp longitude) () "longitude must be a number.")
  (let ((data (signed-request *geo/reverse-geocode* :parameters (prepare* (lat . latitude) (long . longitude) accuracy granularity max-results) :method :GET)))
    (mapcar #'make-location (cdr (assoc :places (cdr (assoc :result data)))))))

(defun parse-geo-attributes (attributes)
  (mapc #'(lambda (pair) (setf (car pair) (format NIL "attribute:~a" (car pair)))) attributes))

(defun geo/search (&key latitude longitude query ip accuracy granularity max-results contained-within attributes)
  "Search for places that can be attached to a statuses/update. Given a latitude and a longitude pair, an IP address, or a name, this request will return a list of all the valid places that can be used as the place_id when updating a status.

According to spec https://dev.twitter.com/docs/api/1.1/get/geo/search" 
  (when granularity (ecase granularity (:POI) (:NEIGHBORHOOD) (:CITY) (:ADMIN) (:COUNTRY)))
  (when latitude (assert (numberp latitude) () "latitude must be a number."))
  (when longitude (assert (numberp longitude) () "longitude must be a number."))
  (setf contained-within (etypecase contained-within
                           (string contained-within)
                           (location (id contained-within))
                           (null)))
  ;; KLUDGE: Twitter sends application/octet-stream content-type for this endpoint for some reason.
  (let* ((data (signed-request *geo/search* :parameters (prepare
                                                         (append
                                                          (parse-geo-attributes attributes)
                                                          `(("lat" . ,latitude)
                                                            ("long" . ,longitude)
                                                            ("query" . ,query)
                                                            ("ip" . ,ip)
                                                            ("accuracy" . ,accuracy)
                                                            ("granularity" . ,granularity)
                                                            ("max_results" . ,max-results)
                                                            ("contained_within" . ,contained-within))))
                                            :method :GET
                                            :drakma-params '(:force-binary T)))
         (data (parse-body (babel:octets-to-string data :encoding :utf-8)
                           '((:content-type . "application/json")))))
    (mapcar #'make-location (cdr (assoc :places (cdr (assoc :result data)))))))

(defun geo/similar-places (latitude longitude name &key contained-within attributes)
  "Locates places near the given coordinates which are similar in name.

According to spec https://dev.twitter.com/docs/api/1.1/get/geo/similar_places"
  (assert (numberp latitude) () "latitude must be a number.")
  (assert (numberp longitude) () "longitude must be a number.")
  (assert (stringp name) () "name must be a string.")
  (let ((data (signed-request *geo/similar-places* :parameters (prepare
                                                                (append
                                                                 (parse-geo-attributes attributes)
                                                                 `(("lat" . ,latitude)
                                                                   ("long" . ,longitude)
                                                                   ("name" . ,name)
                                                                   ("contained_within" . ,contained-within)))) :method :GET)))
    (mapcar #'make-location (cdr (assoc :places (cdr (assoc :result data)))))))

(defgeneric similar-locations (location)
  (:documentation "Performs a GEO/SIMILAR-PLACES request on a location object."))

(defmethod similar-locations ((location location))
  (geo/similar-places (latitude location) (longitude location) (name location)))
