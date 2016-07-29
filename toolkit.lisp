#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defconstant +unix-epoch-difference+  (encode-universal-time 0 0 0 1 1 1970 0) "The universal to unix time difference in seconds.")
(defvar *external-format* :utf-8 "The external format used for encoding/decoding.")

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "DEFCLASS wrapper that sets an automatic INITARG, ACCESSOR and INITFORM (NIL) to
slots that are only a symbol. Slot lists are still treated the same."
  `(defclass ,name ,direct-superclasses
     ,(mapcar #'(lambda (def)
                  (if (listp def)
                      def
                      (list (intern (format NIL "%~a" def))
                            :initarg (intern (string def) "KEYWORD")
                            :accessor def
                            :initform NIL)))
       direct-slots)
     ,@options))
(define-indentation 'defclass* '(4 4 (&whole 2 &rest 1) &rest 2))

(defmacro define-make-* ((class &optional (parametervar (gensym "PARAMS"))) &body assignments)
  "(DEFUN MAKE-* (PARAMETERS) (MAKE-INSTANCE * ..)) wrapper.
Depending on what type the body assignment is, it is expanded into a different
class-slot assignment:

ATOM  ==> ATOM (cdr (assoc ATOM parametervar))
CONS  ==> CAR (cdr (assoc CDR parametervar))
LIST  ==> LIST-ITEMS"
  `(defun ,(intern (format NIL "MAKE-~a" class)) (,parametervar)
     (make-instance
      ',class
      ,@(loop with forms = ()
              for assignment in assignments
              do (setf forms
                       (append forms
                               (if (consp assignment)
                                   (if (consp (cdr assignment))
                                       assignment
                                       (list (car assignment)
                                             `(cdr (assoc ,(cdr assignment) ,parametervar))))
                                   (list assignment
                                         `(cdr (assoc ,assignment ,parametervar))))))
              finally (return forms)))))

(defparameter *month->int-map*
  (let ((table (make-hash-table :test 'equalp)))
    (loop for month in '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
          for num from 1 to 12
          do (setf (gethash month table) num))
    (setf (gethash "sept" table) 9) ;; special handling
    table))

(defun parse-month (string)
  (gethash string *month->int-map*))

(defun parse-twitter-time (string)
  "Parse a string returned by the twitter API that is of
the form \"Fri Mar 26 15:36:12 +0000 2010\" to a local-time
timestamp."
  (destructuring-bind (dow month day h-m-s offset year)
      (split-sequence #\Space (string-trim " " string))
    (declare (ignore dow))
    (destructuring-bind (h m s) (split-sequence #\: h-m-s)
      (local-time:encode-timestamp
       0 (parse-integer s) (parse-integer m) (parse-integer h)
       (parse-integer day) (parse-month month) (parse-integer year)
       :timezone local-time:+gmt-zone+ :offset (parse-integer offset)))))

(defun get-unix-time ()
  "Return the unix timestamp for GMT, as required by OAuth."
  (- (get-universal-time) +unix-epoch-difference+))

(defun generate-nonce ()
  "Generate a NONCE to use for requests. Currently this simply uses a v4-UUID."
  (write-to-string (uuid:make-v4-uuid)))

(defun parse-boolean (value)
  "Parses a string boolean. If the string is one of 
 (T, true, 1), then T is returned, otherwise NIL.
The check is case-insensitive."
  (when (or (string-equal value "true")
            (string-equal value "t")
            (string= value "1"))
    T))

(defun to-keyword (string)
  "Turns a key into a keyword.
Replaces _ with - and uppercases the string, then interns it
into the keyword package. This is useful to parse the request
responses into an alist."
  (let ((name (cl-ppcre:regex-replace-all "_" (string-upcase string) "-")))
    (or (find-symbol name "KEYWORD") (intern name "KEYWORD"))))

(defun from-keyword (keyword)
  "Turns a keyword into a key.
Replaces - with _ and downcases the keyword as a string.
This is useful to parse the request parameters from the
lisp representation into the api representation."
  (cl-ppcre:regex-replace-all "-" (string-downcase keyword) "_"))

(defun url-encode (string &optional (external-format *external-format*))
  "Returns a URL-encoded version of the string STRING using the external format EXTERNAL-FORMAT.

According to spec https://dev.twitter.com/docs/auth/percent-encoding-parameters"
  ;; Adapted from DRAKMA.
  (with-output-to-string (out)
    (loop for octet across (flexi-streams:string-to-octets (or string "") :external-format external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (t (format out "%~2,'0x" (char-code char)))))))

(defun xml-decode (string)
  "Transforms &lt; &gt; &amp; into their proper characters."
  (flet ((r (search replace string)
           (cl-ppcre:regex-replace-all search string replace)))
    (r "&amp;" "&"
       (r "&gt;" ">"
          (r "&lt;" "<" string)))))

(defun xml-encode (string)
  "Transforms & < > into their proper entities."
  (flet ((r (search replace string)
           (cl-ppcre:regex-replace-all search string replace)))
    (r "<" "&lt;"
       (r ">" "&gt;"
          (r "&" "&amp;" string)))))

(defun hmac (string keystring)
  "Returns a base-64 encoded string of the HMAC digest of the given STRING
using the KEYSTRING as HMAC key. The encoding of *external-format* is used 
throughout."
  (let ((hmac (ironclad:make-hmac (flexi-streams:string-to-octets keystring :external-format *external-format*) :SHA1)))
    (ironclad:update-hmac hmac (flexi-streams:string-to-octets string :external-format *external-format*))
    (base64:usb8-array-to-base64-string
     (ironclad:hmac-digest hmac))))

(defun prepare (parameters)
  "Filters out empty key-value pairs and turns all values
into strings, ready to be sent out as request parameters.
This function is DESTRUCTIVE."
  (mapc #'(lambda (pair)
            (setf (car pair) (from-keyword (car pair)))
            (setf (cdr pair) (typecase (cdr pair)
                               (string (cdr pair))
                               (boolean "true")
                               (symbol (string-downcase (cdr pair)))
                               (t (write-to-string (cdr pair))))))
        (delete () parameters :key #'cdr)))

(defmacro prepare* (&rest parameter-names)
  "Creates a PREPARE statement out of the provided variables."
  `(prepare (list ,@(mapcar #'(lambda (a)
                                (if (consp a)
                                    `(cons ,(from-keyword (car a)) ,(cdr a))
                                    `(cons ,(from-keyword a) ,a)))
                            parameter-names))))

(defun flatten-sublists (alist)
  (loop with list = ()
        for (type . elements) in alist
        do (loop for val in elements
                 do (push (cons type val) list))
        finally (return list)))

(defun serialize-object (object)
  "Turns all object slots into an ALIST.
Requires CLOSER-MOP to be installed."
  (assert (asdf:find-system "closer-mop") () "CLOSER-MOP system must be installed.")
  (asdf:load-system "closer-mop")
  (flet ((mop-func (func &rest args)
           (apply (symbol-function (find-symbol func "CLOSER-MOP")) args)))
    (loop for slot in (mop-func "CLASS-SLOTS" (class-of object))
          for name = (mop-func "SLOT-DEFINITION-NAME" slot)
          if (slot-boundp object name)
            collect (cons name (slot-value object name)))))

;; Extend missing function
(defun file-to-base64-string (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8) :if-does-not-exist :error)
    (base64:usb8-array-to-base64-string
     (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8) :fill-pointer t)))
       (setf (fill-pointer seq) (read-sequence seq stream))
       seq))))

(defmacro parse-when-param (parameter function)
  "Helps shorten the writing of conditional object parameters.

Horrible macro, uses implicit PARAMETERS symbol by default."
  (destructuring-bind (param params) (if (listp parameter) parameter (list parameter 'parameters))
    (let ((gensym (gensym)))
      `(when-let ((,gensym (cdr (assoc ,param ,params))))
         (funcall ,function ,gensym)))))

