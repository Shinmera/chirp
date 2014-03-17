#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defclass entity () ()
  (:documentation "Base class for twitter entity objects.

According to spec https://dev.twitter.com/docs/entities"))

(defclass* hashtag (entity)
  (text start end)
  (:documentation "Twitter hashtag entity.

According to spec https://dev.twitter.com/docs/entities#The_hashtags_entity"))

(defclass* media (entity)
  (url id display-url expanded-url start end sizes
   media-url media-url-https media-type source-status)
  (:documentation "Twitter media entity.

According to spec https://dev.twitter.com/docs/entities#The_media_entity"))

(defclass* size ()
  (size height width resize-method)
  (:documentation "Wrapper for size information in media entities."))

(defclass* url (entity)
  (url display-url expanded-url start end)
  (:documentation "Twitter URL entity.

According to spec https://dev.twitter.com/docs/entities#The_urls_entity"))

(defclass* mention (entity)
  (id start end name screen-name)
  (:documentation "Twitter mention entity.

According to spec https://dev.twitter.com/docs/entities#The_user_mentions_entity"))

(defclass* t-symbol (entity)
  (text start end)
  (:documentation "Twitter symbol entity. Name chosen to avoid package clash.

According to spec https://dev.twitter.com/docs/entities#The_symbols_entity"))

(defgeneric make-entity (type parameters))
(defmethod make-entity ((type (eql :hashtags)) parameters)
  (let ((text (cdr (assoc :text parameters)))
        (indices (cdr (assoc :indices parameters))))
    (make-instance 'hashtag :text text :start (first indices) :end (second indices))))

(defmethod make-entity ((type (eql :media)) parameters)
  (let ((id (cdr (assoc :id parameters)))
        (media-url (cdr (assoc :media-url parameters)))
        (media-url-https (cdr (assoc :media-url-https parameters)))
        (url (cdr (assoc :url parameters)))
        (display-url (cdr (assoc :display-url parameters)))
        (expanded-url (cdr (assoc :expanded-url parameters)))
        (type (cdr (assoc :type parameters)))
        (sizes (cdr (assoc :sizes parameters)))
        (source-status (cdr (assoc :source-status-id parameters)))
        (indices (cdr (assoc :indices parameters))))
    (make-instance 'media :id id :start (first indices) :end (second indices)
                          :media-url media-url :media-url-https media-url-https
                          :url url :display-url display-url :expanded-url expanded-url
                          :source-status source-status :media-type type
                          :sizes (loop for (type . params) in sizes
                                       collect (cons type (make-entity type params))))))

(defmethod make-entity ((type (eql :thumb)) parameters)
  (let ((height (cdr (assoc :h parameters)))
        (width (cdr (assoc :w parameters)))
        (resize-method (cdr (assoc :resize parameters))))
    (make-instance 'size :size type :height height :width width :resize-method resize-method)))

(defmethod make-entity ((type (eql :small)) parameters)
  (let ((height (cdr (assoc :h parameters)))
        (width (cdr (assoc :w parameters)))
        (resize-method (cdr (assoc :resize parameters))))
    (make-instance 'size :size type :height height :width width :resize-method resize-method)))

(defmethod make-entity ((type (eql :medium)) parameters)
  (let ((height (cdr (assoc :h parameters)))
        (width (cdr (assoc :w parameters)))
        (resize-method (cdr (assoc :resize parameters))))
    (make-instance 'size :size type :height height :width width :resize-method resize-method)))

(defmethod make-entity ((type (eql :large)) parameters)
  (let ((height (cdr (assoc :h parameters)))
        (width (cdr (assoc :w parameters)))
        (resize-method (cdr (assoc :resize parameters))))
    (make-instance 'size :size type :height height :width width :resize-method resize-method)))

(defmethod make-entity ((type (eql :urls)) parameters)
  (let ((url (cdr (assoc :url parameters)))
        (display-url (cdr (assoc :display-url parameters)))
        (expanded-url (cdr (assoc :expanded-url parameters)))
        (indices (cdr (assoc :indices parameters))))
    (make-instance 'url :url url :start (first indices) :end (second indices) :display-url display-url :expanded-url expanded-url)))

(defmethod make-entity ((type (eql :user-mentions)) parameters)
  (let ((id (cdr (assoc :id parameters)))
        (indices (cdr (assoc :indices parameters)))
        (name (cdr (assoc :name parameters)))
        (screen-name (cdr (assoc :screen-name parameters))))
    (make-instance 'mention :id id :start (first indices) :end (second indices) :screen-name screen-name :name name)))

(defmethod make-entity ((type (eql :symbols)) parameters)
  (let ((text (cdr (assoc :text parameters)))
        (indices (cdr (assoc :indices parameters))))
    (make-instance 't-symbol :text text :start (first indices) :end (second indices))))

(defmethod make-entity ((type (eql :url)) parameters)
  (mapcar #'(lambda (a) (make-entity (car parameters) a)) (cdr parameters)))

(defmethod make-entity ((type (eql :description)) parameters)
  (mapcar #'(lambda (a) (make-entity (car parameters) a)) (cdr parameters)))

(defun make-entities (parameters)
  (loop for (type . entities) in parameters
        collect (cons type (mapcar #'(lambda (a) (make-entity type a)) entities))))
