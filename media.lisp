(in-package #:org.tymoonnext.chirp)

(defvar *media/upload* "https://upload.twitter.com/1.1/media/upload.json")
(defvar *media/metadata/create* "https://upload.twitter.com/1.1/media/metadata/create.json")
(defvar *media/subtitles/create* "https://upload.twitter.com/1.1/media/subtitles/create.json")
(defvar *media/subtitles/delete* "https://upload.twitter.com/1.1/media/subtitles/delete.json")

(defclass* media-object ()
  (id id-string size expires-after-secs image video processing-info))

(defmethod print-object ((media media-object) stream)
  (print-unreadable-object (media stream :type T)
    (format stream "~a" (id media))))

(define-make-* (media-object parameters)
  :size :expires-after-secs :image :video :processing-info
  (:id (cdr (assoc :media-id parameters)))
  (:id-string (cdr (assoc :media-id-string parameters))))

(defun media-category (category)
  (ecase category
    ((NIL) NIL)
    (:tweet-image "TweetImage")
    (:tweet-video "TweetVideo")
    (:tweet-gif "TweetGif")
    (:dm-image "DmImage")
    (:dm-video "DmVideo")
    (:dm-gif "DmGif")
    (:subtitles "Subtitles")))

(defun pathname-media-type (path)
  (let ((type (pathname-type path)))
    (cond ((or (string-equal "jpg" type) (string-equal "jpeg" type))
           "image/jpeg")
          ((string-equal "gif" type)
           "image/gif")
          ((string-equal "png" type)
           "image/png")
          ((string-equal "webp" type)
           "image/webp")
          ((string-equal "mp4" type)
           "video/mp4")
          (T
           (error "Unsupported media type ~s" type)))))

(defun media/upload (payload &key start end media-type category additional-owners)
  (let ((category (media-category category))
        (max-buffer (* 1024 1024 5)))
    (etypecase payload
      ((or string pathname)
       (with-open-file (stream payload :direction :input :element-type '(unsigned-byte 8))
         (let* ((length (file-length stream))
                (media-type (or media-type (pathname-media-type payload)))
                (buffer (make-array (min length max-buffer) :element-type '(unsigned-byte 8)))
                (media (media/upload/init length media-type :category category :additional-owners additional-owners)))
           (loop while (< 0 length)
                 for segment from 0
                 for read = (read-sequence buffer stream)
                 do (decf length read)
                    (let ((buffer (if (= read (length buffer))
                                      buffer
                                      (make-array read :element-type '(unsigned-byte 8) :displaced-to buffer))))
                      (media/upload/append media buffer segment)))
           (media/upload/finalize media))))
      ((vector (unsigned-byte 8))
       (let* ((start (or start 0))
              (end (or end (length payload)))
              (length (- end start))
              (media (media/upload/init length media-type :category category :additional-owners additional-owners))
              (i 0))
         (loop while (< i length)
               for segment from 0
               for size = (min max-buffer (- end i))
               for buffer = (make-array size :element-type '(unsigned-byte 8) :displaced-to payload :displaced-index-offset i)
               do (incf i size)
                  (media/upload/append media buffer segment))
         (media/upload/finalize media))))))

(defun media/upload/init (total-bytes media-type &key category additional-owners)
  (make-media-object (signed-request *media/upload* :parameters (prepare* (command . "INIT") total-bytes media-type (media-category . category) (additional-owners . (when additional-owners (format NIL "~{~a~^,~}" additional-owners))))
                                                    :method :post)))

(defun media/upload/append (media payload segment)
  (signed-data-request *media/upload* :data-parameters `(("media" . ,payload))
                                      :parameters (prepare* (command . "APPEND") (media-id . (id media)) (segment-index . segment))
                                      :method :post))

(defun media/upload/status (media)
  (make-media-object (signed-request *media/upload* :parameters (prepare* (command . "STATUS") (media-id . (id media))) :method :get)))

(defun media/upload/finalize (media)
  (make-media-object (signed-request *media/upload* :parameters (prepare* (command . "FINALIZE") (media-id . (id media))) :method :post)))
