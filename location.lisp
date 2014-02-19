#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defclass location ()
  ((%place :initarg :place :accessor place)
   (%lat :initarg :lat :accessor lat)
   (%lng :initarg :lng :accessor lng))
  (:documentation ""))

(defmethod print-object ((location location) stream)
  (print-unreadable-object (location stream :type T)
    (format stream "~a/~a ~a" (lat location) (lng location) (place location)))
  location)
