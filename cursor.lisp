(in-package #:org.tymoonnext.chirp)

(defclass* cursor ()
  (id url parameters data request-method)
  (:documentation "Cursor object to traverse cursored sets.

According to https://dev.twitter.com/docs/misc/cursoring"))

(defun cursored-request (uri &key parameters (method :GET))
  "Performs a signed-request returning a cursor instance tailored for it."
  (cursor-request
   (make-instance
    'cursor :id -1 :url uri :parameters parameters :request-method method)))

(defgeneric cursor-request (cursor)
  (:documentation "Perform the currently stored cursor request and save the data in the cursor object."))

(defmethod cursor-request ((cursor cursor))
  (let ((data (signed-request (url cursor) :parameters (append (parameters cursor)
                                                               `(("cursor" . ,(write-to-string (id cursor))))) :method (request-method cursor))))
    (setf (data cursor) data)
    (values cursor data)))

(defgeneric cursor-next (cursor)
  (:documentation "Moves the cursor to the next iteration if possible and saves the data in the cursor object.
If no additional data is available, NIL is returned instead."))

(defmethod cursor-next ((cursor cursor))
  (assert (data cursor) () "Cursor contains no data. Did you perform a CURSOR-REQUEST yet?")
  (let ((next-id (cdr (assoc :next-cursor (data cursor)))))
    (assert (not (null next-id)) () "No next cursor id found in data!")
    (when (< 0 (cdr (assoc :next-cursor (data cursor))))
      (setf (id cursor) next-id)
      (cursor-request cursor))))

(defgeneric cursor-previous (cursor)
  (:documentation "Moves the cursor to the previous iteration if possible and saves the data in the cursor object.
If no additional data is available, NIL is returned instead."))

(defmethod cursor-previous ((cursor cursor))
  (assert (data cursor) () "Cursor contains no data. Did you perform a CURSOR-REQUEST yet?")
  (let ((previous-id (cdr (assoc :previous-cursor (data cursor)))))
    (assert (not (null previous-id)) () "No previous cursor id found in data!")
    (when (< 0 (cdr (assoc :previous-cursor (data cursor))))
      (setf (id cursor) previous-id)
      (cursor-request cursor))))

(defmacro do-cursor ((data-var uri &key parameters (method :GET) (cursor-var (gensym "CURSOR"))) &body body)
  "Iterates over all data sets of a cursor, binding the request data to DATA-VAR on each iteration.
Returns the cursor at its end position. "
  `(loop with ,cursor-var = (cursored-request ,uri :parameters ,parameters :method ,method)
         for ,data-var = (data ,cursor-var)
         do (progn ,@body)
         while (cursor-next ,cursor-var)
         finally (return ,cursor-var)))

(defun map-cursor (function parameter uri &key parameters (method :GET))
  "Applies FUNCTION to each element of the designated parameter while going through the cursor.
Returns the collected return values of the FUNCTION calls."
  (let ((result ()))
    (do-cursor (data uri :parameters parameters :method method)
      (appendf result (mapcar function (if parameter (cdr (assoc parameter data)) data))))
    result))

(defun cursor-collect (parameter uri &key parameters (method :GET))
  "Collects the given parameter into one list by APPEND."
  (let ((result ()))
    (do-cursor (data uri :parameters parameters :method method)
      (appendf result (if parameter (cdr (assoc parameter data)) data)))
    result))
