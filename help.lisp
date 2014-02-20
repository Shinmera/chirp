#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defvar *help/configuration* "https://api.twitter.com/1.1/help/configuration.json")
(defvar *help/languages* "https://api.twitter.com/1.1/help/languages.json")
(defvar *help/privacy* "https://api.twitter.com/1.1/help/privacy.json")
(defvar *help/tos* "https://api.twitter.com/1.1/help/tos.json")
(defvar *application/rate-limit-status* "https://api.twitter.com/1.1/application/rate_limit_status.json")

(defclass* configuration ()
  (photo-size-limit photo-sizes short-url-length short-url-length-https
   non-username-paths max-media-per-upload characters-reserved-per-media)
  (:documentation "Class representation of the twitter configuration object.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/configuration"))

(defun make-configuration (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'configuration
     :photo-size-limit (param :photo-size-limit)
     :photo-sizes (loop for (type . params) in (param :photo-sizes)
                        collect (cons type (make-entity type params)))
     :short-url-length (param :short-url-length)
     :short-url-length-https (param :short-url-length-https)
     :non-username-paths (param :non-username-paths)
     :max-media-per-upload (param :max-media-per-upload)
     :characters-reserved-per-media (param :characters-reserved-per-media))))

(defclass* language ()
  (name code status)
  (:documentation "Class representation of the twitter language object.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/languages"))

(defmethod print-object ((language language) stream)
  (print-unreadable-object (language stream :type T)
    (format stream "~a" (name language)))
  language)

(defun make-language (parameters)
  (flet ((param (place) (cdr (assoc place parameters))))
    (make-instance
     'language
     :name (param :name)
     :code (param :code)
     :status (param :status))))

(defclass* resource ()
  (address remaining reset limit)
  (:documentation "Class representation of a twitter resource objkect.

According to spec https://dev.twitter.com/docs/api/1.1/get/application/rate_limit_status"))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type T)
    (format stream "~a (~d/~d)" (address resource) (remaining resource) (limit resource)))
  resource)

(defun make-resource (parameters)
  (flet ((param (place) (cdr (assoc place (cdr parameters)))))
    (make-instance
     'resource
     :address (car parameters)
     :remaining (param :remaining)
     :reset (local-time:unix-to-timestamp (param :reset))
     :limit (param :limit))))

(defun help/configuration ()
  "Returns the current configuration used by Twitter including twitter.com slugs which are not usernames, maximum photo resolutions, and t.co URL lengths.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/configuration"
  (make-configuration (signed-request *help/configuration* :method :GET)))

(defun help/languages ()
  "Returns the list of languages supported by Twitter along with their ISO 639-1 code. The ISO 639-1 code is the two letter value to use if you include lang with any of your requests.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/languages"
  (mapcar #'make-language (signed-request *help/languages* :method :GET)))

(defun help/privacy ()
  "Returns Twitter's Privacy Policy.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/privacy"
  (cdr (assoc :privacy (signed-request *help/privacy* :method :GET))))

(defun help/tos ()
  "Returns the Twitter Terms of Service in the requested format. These are not the same as the Developer Rules of the Road.

According to spec https://dev.twitter.com/docs/api/1.1/get/help/tos"
  (cdr (assoc :tos (signed-request *help/tos* :method :GET))))

(defun application/rate-limit-status (resources)
  "Returns the current rate limits for methods belonging to the specified resource families.

According to spec https://dev.twitter.com/docs/api/1.1/get/application/rate_limit_status"
  (setf resources (etypecase resources
                    (string resources)
                    (cons (format NIL "~{~a~^,~}" resources))))
  (let ((data (signed-request *application/rate-limit-status* :parameters `(("resources" . ,resources)) :method :GET)))
    (mapc #'(lambda (resource)
              (setf (cdr resource) (mapcar #'make-resource (cdr resource))))
          (cdr (assoc :resources data)))))

(defun valid-language-p (language)
  "Returns T if the given language code is a language covered by twitter.

See HELP/LANGUAGES."
  (loop for lang in (help/languages)
        if (string-equal language (code lang))
          return T))
