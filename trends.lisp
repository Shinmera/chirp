#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *trends/place* "https://api.twitter.com/1.1/trends/place.json")
(defvar *trends/available* "https://api.twitter.com/1.1/trends/available.json")
(defvar *trends/closest* "https://api.twitter.com/1.1/trends/closest.json")

(defclass trend ()
  ((%events :initarg :events :accessor events)
   (%name :initarg :name :accessor name)
   (%promoted-content :initarg :promoted-content :accessor promoted-content)
   (%query :initarg :query :accessor query)
   (%url :initarg :url :accessor url))
  (:default-initargs :events NIL :name NIL :promoted-content NIL
                     :query NIL :url NIL)
  (:documentation "Class representation of a twitter trend object."))

(defmethod print-object ((trend trend) stream)
  (print-unreadable-object (trend stream :type T)
    (format stream "~a" (name trend)))
  trend)

(defclass trend-location ()
  ((%country :initarg :country :accessor country)
   (%country-code :initarg :country-code :accessor country-code)
   (%name :initarg :name :accessor name)
   (%parent :initarg :parent :accessor parent)
   (%place-code :initarg :place-code :accessor place-code)
   (%place-name :initarg :place-name :accessor place-name)
   (%url :initarg :url :accessor url)
   (%woeid :initarg :woeid :accessor woeid))
  (:default-initargs :country NIL :country-code NIL :name NIL :parent NIL
                     :place-code NIL :place-name NIL :url NIL :woeid NIL)
  (:documentation "Class representation of a twitter trend location object."))

(defmethod print-object ((trend-location trend-location) stream)
  (print-unreadable-object (trend-location stream :type T)
    (format stream "~a #~d" (name trend-location) (woeid trend-location)))
  trend-location)

(defun make-trend (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'trend
     :events (param :events)
     :name (param :name)
     :promoted-content (param :promoted-content)
     :query (param :query)
     :url (param :url))))

(defun make-trend-location (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'trend-location
     :country (param :country)
     :country-code (param :countrycode)
     :name (param :name)
     :parent (param :parentid)
     :place-code (cdr (assoc :code (param :placetype)))
     :place-name (cdr (assoc :name (param :placetype)))
     :url (param :url)
     :woeid (param :woeid))))

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
