#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *geo/id* "https://api.twitter.com/1.1/geo/id/~a.json")
(defvar *geo/reverse-geocode* "https://api.twitter.com/1.1/geo/reverse_geocode.json")
(defvar *geo/search* "https://api.twitter.com/1.1/geo/search.json")
(defvar *geo/similar-places* "https://api.twitter.com/1.1/geo/similar_places.json")

(defclass location ()
  ((%id :initarg :id :accessor id)
   (%name :initarg :name :accessor name)
   (%full-name :initarg :full-name :accessor full-name)
   (%location-type :initarg :location-type :accessor location-type)
   (%latitude :initarg :latitude :accessor latitude)
   (%longitude :initarg :longitude :accessor longitude)
   (%url :initarg :url :accessor url)
   (%country-name :initarg :country-name :accessor country-name)
   (%country-code :initarg :country-code :accessor country-code)
   (%bounding-box :initarg :bounding-box :accessor bounding-box)
   (%poly-lines :initarg :poly-lines :accessor poly-lines)
   (%contained-within :initarg :contained-within :accessor contained-within))
  (:default-initargs :id NIL :name NIL :full-name NIL :location-type NIL
                     :latitude NIL :longitude NIL :url NIL :country-name NIL
                     :country-code NIL :bounding-box NIL :poly-lines NIL
                     :contained-within NIL)
  (:documentation "Twitter object containing Locational data. Used for the Places & Geo API."))

(defmethod print-object ((location location) stream)
  (print-unreadable-object (location stream :type T)
    (format stream "~a #~a" (name location)  (id location)))
  location)

(defclass geometry ()
  ((%shape :initarg :shape :accessor shape)
   (%coordinates :initarg :coordinates :accessor coordinates))
  (:default-initargs :shape (error "Shape type required") :coordinates NIL)
  (:documentation "Object for locations containing geometrical shape data."))

(defmethod print-object ((geometry geometry) stream)
  (print-unreadable-object (geometry stream :type T :identity T)
    (format stream "~a" (shape geometry))))

(defun make-geometry (parameters)
  (make-instance
   'geometry
   :shape (cdr (assoc :type parameters))
   :coordinates (cdr (assoc :coordinates parameters))))

(defun make-location (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'location
     :id (param :id)
     :name (param :name)
     :full-name (param :full-name)
     :location-type (param :place-type)
     :latitude (param :lat)
     :longitude (param :long)
     :url (param :url)
     :country-name (param :country)
     :country-code (param :country-code)
     :bounding-box (when (param :bounding-box)
                     (make-geometry (param :bounding-box)))
     :poly-lines (param :poly-lines)
     :contained-within (when (param :contained-within)
                         (make-location (param :contained-wihtin))))))

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
  (let ((data (signed-request *geo/reverse-geocode* :parameters (prepare `(("lat" . ,latitude)
                                                                           ("long" . ,longitude)
                                                                           ("accuracy" . ,accuracy)
                                                                           ("granularity" . ,granularity)
                                                                           ("max_results" . ,max-results))) :method :GET)))
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
  (let ((data (signed-request *geo/search* :parameters (prepare
                                                        (append
                                                         (parse-geo-attributes attributes)
                                                         `(("lat" . ,latitude)
                                                           ("long" . ,longitude)
                                                           ("query" . ,query)
                                                           ("ip" . ,ip)
                                                           ("accuracy" . ,accuracy)
                                                           ("granularity" . ,granularity)
                                                           ("max_results" . ,max-results)
                                                           ("contained_within" . ,contained-within)))) :method :GET)))
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

(defgeneric geo/similar (location)
  (:documentation "Performs a GEO/SIMILAR-PLACES request on a location object."))

(defmethod geo/similar ((location location))
  (geo/similar-places (latitude location) (longitude location) (name location)))
