(in-package #:org.tymoonnext.chirp)

(defvar *collections/entries* "https://api.twitter.com/1.1/collections/entries.json")
(defvar *collections/list* "https://api.twitter.com/1.1/collections/list.json")
(defvar *collections/show* "https://api.twitter.com/1.1/collections/show.json")
(defvar *collections/create* "https://api.twitter.com/1.1/collections/create.json")
(defvar *collections/destroy* "https://api.twitter.com/1.1/collections/destroy.json")
(defvar *collections/entries/add* "https://api.twitter.com/1.1/collections/entries/add.json")
(defvar *collections/entries/curate* "https://api.twitter.com/1.1/collections/entries/curate.json")
(defvar *collections/entries/move* "https://api.twitter.com/1.1/collections/entries/move.json")
(defvar *collections/entries/remove* "https://api.twitter.com/1.1/collections/entries/remove.json")
(defvar *collections/update* "https://api.twitter.com/1.1/collections/update.json")

(defclass* collection ()
  (id collection-type visibility order associated-url url users tweets user-id name)
  (:documentation "Class representation of a collection of tweets.

According to spec https://developer.twitter.com/en/docs/tweets/curate-a-collection"))

(defmethod print-object ((collection collection) stream)
  (print-unreadable-object (collection stream :type T)
    (format stream "~a " (name collection))
    (format stream "#~d" (id collection)))
  collection)

(define-make-* (collection parameters)
  (:id (cdadr (assoc :response parameters)))
  (:collection-type
   (cdr (assoc :collection-type (cdadr (assoc :timelines (cdr (assoc :objects parameters)))))))
  (:visibility
   (cdr (assoc :visibility (cdadr (assoc :timelines (cdr (assoc :objects parameters)))))))
  (:order
   (cdr (assoc :timeline-order (cdadr (assoc :timelines (cdr (assoc :objects parameters)))))))
  (:associated-url
   (cdr (assoc :url (cdadr (assoc :timelines (cdr (assoc :objects parameters)))))))
  (:url
   (cdr (assoc :custom-timeline-url (cdadr (assoc :timelines (cdr (assoc :objects parameters)))))))
  (:users (mapcar (lambda (usr-param)
                    (make-user (cdr usr-param)))
                  (cdr (assoc :users (cdr (assoc :objects parameters))))))
  :tweets
  (:user-id
   (parse-integer (cdr (assoc :user-id (cdadr (assoc :timelines (cdr (assoc :objects parameters))))))))
  (:name (cdr (assoc :name (cdadr (assoc :timelines (cdr (assoc :objects parameters))))))))

(defun collections/entries (collection &key count max-position min-position)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/get-collections-entries"

  (let* ((id (id collection))
         (params (prepare* id count max-position min-position))
         (res (signed-request *collections/entries* :parameters params :method :GET))
         (objs (cdr (assoc :objects res)))
         (tweets (mapcar (lambda (tw)
                           (make-status (cdr tw)))
                         (cdr (assoc :tweets objs))))
         (users (mapcar (lambda (user)
                          (make-user (cdr user)))
                        (cdr (assoc :users objs)))))

    (dolist (tw tweets tweets)
      (setf (user tw)
            (find-if (lambda (user) (= (id user) (id (user tw)))) users)))))

(defun %make-collection (tl-param)
  (make-instance 'collection
                 :id (string-downcase (symbol-name (car tl-param)))
                 :collection-type (cdr (assoc :collection-type (cdr tl-param)))
                 :visibility (cdr (assoc :visibility (cdr tl-param)))
                 :order (cdr (assoc :timeline-order (cdr tl-param)))
                 :url (cdr (assoc :custom-timeline-url (cdr tl-param)))
                 :user-id (parse-integer (cdr (assoc :user-id (cdr tl-param))))
                 :name (cdr (assoc :name (cdr tl-param)))))

(defun collections/list (user-id/screen-name &key tweet-id count cursor)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/get-collections-list"

  (let* ((user-id (typecase user-id/screen-name (fixnum user-id/screen-name)))
         (screen-name (typecase user-id/screen-name (string user-id/screen-name)))
         (params (prepare* user-id screen-name tweet-id count cursor))
         (res (signed-request *collections/list* :parameters params :method :GET)))
    (mapcar #'%make-collection (cdr (assoc :timelines (cdr (assoc :objects res)))))))

(defun collections/show (id)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/get-collections-show"
  (make-collection (signed-request *collections/show* :parameters (prepare* id)
                                                      :method :GET)))

(defun collections/create (name &key description url timeline-order)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-create

timeline-order:
  curation_reverse_chron : order added (default)
  tweet_chron            : oldest first
  tweet_reverse_chron    : most recent first
"

  (make-collection (signed-request *collections/create*
                                   :parameters (prepare* name description url timeline-order)
                                   :method :POST)))

(defun collections/destroy (id)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-destroy"

  (cdr (assoc :destroyed (signed-request *collections/destroy*
                                         :parameters (prepare* id)
                                         :method :POST))))

(defun collections/add (collection tweet-id &key relative-to above)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-entries-add"
  (let* ((id (id collection))
         (params (prepare* id tweet-id relative-to above)))
    (signed-request *collections/entries/add* :parameters params :method :POST)))

(defun make-curate-request (timeline-id op/tweet-id-alist)
  (assert (every (lambda (op/tweet) (or (eq (car op/tweet) :add) (eq (car op/tweet) :remove)))
                 op/tweet-id-alist))
  (flet ((ahash (alist)
           (alexandria:alist-hash-table alist :test #'equal)))
    (with-output-to-string (s)
      (yason:encode
       (ahash
        (list (cons "id" timeline-id)
              (cons "changes"
                    (mapcar
                     (lambda (op/tweet)
                       (ecase (car op/tweet)
                         (:add (ahash `(("op" . "add")
                                        ("tweet_id" . ,(format nil "~d" (cadr op/tweet))))))
                         (:remove (ahash `(("op" . "remove")
                                           ("tweet_id" . ,(format nil "~d" (cadr op/tweet))))))))
                     op/tweet-id-alist))))
       s))))

(defun collections/curate (collection op/tweet-id-alist)
  "Curate tweets with alist that consisted of operations and tweet-id.

Example: 
(defparameter *col* (collection/show \"custom-123456789\"))

(collections/curate *col*
   '((:add    1234)
     (:add    4321)
     (:remove 1111)
     (:add    2222)))
; => DONE

According to spec https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-entries-curate"
  (let* ((parameters
           (signed-request
            *collections/entries/curate*
            :method :POST
            :drakma-params (list :content
                                 (make-curate-request (id collection) op/tweet-id-alist)
                                 :content-type "application/json")))
         (err (cdr (assoc :errors (cdr (assoc :response parameters))))))
    (if err (error "Error in collection/curate : ~A" err)
        'done)))

(defun collections/move (collection tweet-id relative-to &key above)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-entries-move"
  (let* ((id (id collection))
         (params (prepare* id tweet-id relative-to above)))
    (signed-request *collections/entries/move* :parameters params :method :POST)))

(defun collections/remove (collection tweet-id)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-entries-remove"
  (let* ((id (id collection))
         (params (prepare* id tweet-id)))
    (signed-request *collections/entries/remove* :parameters params :method :POST)))

(defun collections/update (id &key name description url)
  "https://developer.twitter.com/en/docs/tweets/curate-a-collection/api-reference/post-collections-update"

  (signed-request *collections/update*
                  :parameters (prepare* id name description url)
                  :method :POST))
