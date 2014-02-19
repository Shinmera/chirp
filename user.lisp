#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defclass user ()
  ((%id :initarg :id :accessor id)
   (%name :initarg :name :accessor name)
   (%screen :initarg :screen :accessor screen)
   (%created :initarg :created :accessor created)
   (%profile :initarg :profile :accessor profile)
   (%location :initarg :location :accessor location))
  (:documentation ""))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a #~d" (screen user) (id user)))
  user)

(defclass profile ()
  ((%user :initarg :user :accessor user))
  (:documentation ""))

(defmethod print-object ((profile profile) stream)
  (print-unreadable-object (profile stream :type T)
    (format stream "~a #~d" (user profile)))
  profile)
