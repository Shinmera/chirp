#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.chirp)

(defconstant +unix-epoch-difference+  (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *external-format* :utf-8)
(defvar *oauth-consumer-key* NIL)
(defvar *oauth-consumer-secret* NIL)
(defvar *oauth-token* NIL)
(defvar *oauth-token-secret* NIL)
(defvar *oauth-signature-method* "HMAC-SHA1")
(defvar *oauth-version* "1.0")
(defvar *oauth/request-token* "https://api.twitter.com/oauth/request_token")
(defvar *oauth/authenticate* "https://api.twitter.com/oauth/authenticate")
(defvar *oauth/authorize* "https://api.twitter.com/oauth/authorize")
(defvar *oauth/access-token* "https://api.twitter.com/oauth/access_token")
(defvar *server-port* 8989)

(define-condition oauth-error (error) ())

(define-condition oauth-parameter-missing (oauth-error)
  ((%parameter :initarg :parameter :accessor parameter))
  (:report (lambda (c s) (format s "The ~a parameter is required for OAuth, but is not set." (parameter c)))))

(define-condition oauth-request-error (oauth-error)
  ((%http-status :initarg :status :accessor http-status)
   (%http-body :initarg :body :accessor http-body)
   (%http-headers :initarg :headers :accessor http-headers)
   (%target-url :initarg :url :accessor target-url)
   (%target-method :initarg :method :accessor target-method)
   (%target-parameters :initarg :parameters :accessor target-parameters)
   (%target-headers :initarg :sent-headers :accessor target-headers))
  (:report (lambda (c s) (format s "OAuth Request Failed: Status code ~d when requesting ~a :~%~s"
                                 (http-status c) (target-url c) (http-body c)))))

(defun get-unix-time ()
  "Return the unix timestamp for GMT, as required by OAuth."
  (- (get-universal-time) +unix-epoch-difference+))

(defun generate-nonce ()
  "Generate a NONCE to use for requests. Currently this simply uses a v4-UUID."
  (write-to-string (uuid:make-v4-uuid)))

(defun parse-boolean (value)
  (when (string= value "true") T))

(defun to-keyword (string)
  (intern (cl-ppcre:regex-replace-all "_" (string-upcase string) "-") "KEYWORD"))

(defun oauth-response->alist (body &optional spec)
  "Turn an oauth-response into an ALIST."
  (mapcar #'(lambda (assignment)
              (let* ((pair (split-sequence #\= assignment))
                     (key (to-keyword (first pair)))
                     (val (second pair))
                     (parser (cdr (assoc key spec))))
                (cons key (if parser (funcall parser val) val))))
          (split-sequence #\& body)))

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

(defun hmac (string keystring)
  (let ((hmac (ironclad:make-hmac (flexi-streams:string-to-octets keystring :external-format *external-format*) :SHA1)))
    (ironclad:update-hmac hmac (flexi-streams:string-to-octets string :external-format *external-format*))
    (base64:usb8-array-to-base64-string
     (ironclad:hmac-digest hmac))))

(defun signature-format-parameter (s param &rest rest)
  (declare (ignore rest))
  (format s "~a=~a" (car param) (cdr param)))

(defun create-signature (method url parameters)
  "Create an OAuth signature for a request.
This requires at least the *oauth-consumer-secret* to be bound properly, and
usually the *oauth-token-secret* as well.

According to spec https://dev.twitter.com/docs/auth/creating-signature"
  (assert (not (null *oauth-consumer-secret*)) (*oauth-consumer-secret*)
          'oauth-parameter-missing :parameter '*oauth-consumer-secret*)
  (let ((prepared-parameters (sort (loop for (key . val) in parameters
                                         collect (cons (url-encode key)
                                                       (url-encode val)))
                                   #'string< :key #'car)))
    (let ((parameter-string (format NIL "~{~/chirp::signature-format-parameter/~^&~}" prepared-parameters)))
      (let ((base (format NIL "~a&~a&~a" (string-upcase method) (url-encode url) (url-encode parameter-string)))
            (signing-key (format NIL "~a&~a" *oauth-consumer-secret* (or *oauth-token-secret* ""))))
        (hmac base signing-key)))))

(defun make-signed (method url parameters)
  "Returns the signed version of the parameters.
Simply generates a signature and appends the proper parameter."
  (cons (cons "oauth_signature" (create-signature method url parameters))
        parameters))

(defun authorization-format-parameter (s param &rest rest)
  (declare (ignore rest))
  (format s "~a=~s" (url-encode (car param)) (url-encode (cdr param))))

(defun create-authorization-header (parameters)
  ""
  (format NIL "OAuth ~{~/chirp::authorization-format-parameter/~^, ~}"
          (sort parameters #'string< :key #'car)))

(defun signed-request (request-url &key parameters oauth-parameters (method :POST))
  "Issue a signed request against the API.
This requires the *oauth-consumer-key*, *oauth-signature-method*,
*oauth-version* and at least *oauth-consumer-secret* to be set.
See CREATE-SIGNATURE.
For return values see DRAKMA:HTTP-REQUEST

According to spec https://dev.twitter.com/docs/auth/authorizing-request"
  (assert (not (null *oauth-consumer-key*)) (*oauth-consumer-key*)
          'oauth-parameter-missing :parameter '*oauth-consumer-key*)
  (assert (not (null *oauth-signature-method*)) (*oauth-signature-method*)
          'oauth-parameter-missing :parameter '*oauth-signature-method*)
  (assert (not (null *oauth-version*)) (*oauth-version*)
          'oauth-parameter-missing :parameter '*oauth-version*)
  (let* ((oauth-parameters (append
                            oauth-parameters
                            `(("oauth_consumer_key" . ,*oauth-consumer-key*)
                              ("oauth_nonce" . ,(generate-nonce))
                              ("oauth_signature_method" . ,*oauth-signature-method*)
                              ("oauth_timestamp" . ,(write-to-string (get-unix-time)))
                              ("oauth_version" . ,*oauth-version*))
                            (when *oauth-token* `(("oauth_token" . ,*oauth-token*)))))
         (oauth-parameters (make-signed method request-url oauth-parameters))
         (headers `(("Authorization" . ,(create-authorization-header oauth-parameters)))))
    (let ((result (multiple-value-list (drakma:http-request
                                        request-url :method method :parameters parameters :additional-headers headers))))
      (if (= (second result) 200)
          (values-list result)
          (error 'oauth-request-error
                 :body (first result) :status (second result) :headers (third result)
                 :url request-url :method method :parameters parameters :sent-headers headers)))))

(defun request-token (callback)
  "Query for a request token using the specified callback.
Returns an ALIST containing :OAUTH-TOKEN, :OAUTH-TOKEN-SECRET and 
:OAUTH-CALLBACK-CONFIRMED, the first two being strings and the last a boolean.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (oauth-response->alist
   (signed-request *oauth/request-token* :oauth-parameters `(("oauth_callback" . ,callback)))
   `((:OAUTH-CALLBACK-CONFIRMED . ,#'parse-boolean))))

(defun pin-request-token ()
  "Query for a PIN based request token.
Seee CALLBACK-REQUEST-TOKEN.

According to spec https://dev.twitter.com/docs/auth/pin-based-authorization"
  (request-token "oob"))

(defun initiate-redirect-authentication (callback-url)
  "Initiate the authentication through the redirect mechanism.
Returns an URL that the user has to open in the browser.
Upon successful authentication, the page should redirect back
to the specified callback url. This callback endpoint should then
pass the proper parameters to COMPLETE-AUTHENTICATION.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (let ((data (request-token callback-url)))
    (setf *oauth-token* (cdr (assoc :oauth-token data))
          *oauth-token-secret* (cdr (assoc :oauth-token-secret data)))
    (format NIL "~a?oauth_token=~a" *oauth/authorize* *oauth-token*)))

(defun initiate-pin-authentication ()
  "Initiate the authentication through the PIN mechanism.
Returns an URL that the user has to open in the browser.
This page should, upon successful authentication, return a PIN
that has to be initialized by passing it to COMPLETE-AUTHENTICATION.

According to spec https://dev.twitter.com/docs/auth/pin-based-authorization"
  (initiate-redirect-authentication "oob"))

(defun initiate-server-authentication ()
  "Initiate the authentication through the server mechanism.
This is the same as the redirect mechanism, except it starts up
a Hunchentoot server automatically and handles the callback for
you. The server will be started on *SERVER-PORT* and will be shut
down automatically after a single request."
  (assert (asdf:find-system "hunchentoot") () "Please install hunchentoot before using this authentication method.")
  (asdf:load-system "hunchentoot")
  (labels ((ht-symb (name)
             (find-symbol name "HUNCHENTOOT"))
           (ht-func (name &rest params)
             (apply (symbol-function (ht-symb name)) params)))
    (let ((server (ht-func "START" (make-instance (ht-symb "EASY-ACCEPTOR") :port *server-port*)))
          (standard-output *standard-output*))
      (flet ((dispatcher (request)
               (unless (cl-ppcre:scan  "favicon\.ico$" (ht-func "SCRIPT-NAME" request))
                 (lambda (&rest args)
                   (declare (ignore args))
                   (unwind-protect
                        (progn
                          (complete-authentication
                           (ht-func "GET-PARAMETER" "oauth_verifier")
                           (ht-func "GET-PARAMETER" "oauth_token"))
                          (format standard-output "CHIRP-OAUTH: Authentication completed."))
                     (format standard-output "CHIRP-OAUTH: Shutting down server.")
                     (ht-func "STOP" server))))))
        (push #'dispatcher (symbol-value (ht-symb "*DISPATCH-TABLE*")))
        (initiate-redirect-authentication (format NIL "http://localhost:~d/callback" *server-port*))))))

(defun initiate-authentication (&key (method :PIN) (consumer-key *oauth-consumer-key*) (consumer-secret *oauth-consumer-secret*))
  "Starts the authentication process and returns an URL that the user has to visit.
METHOD can be one of :PIN :SERVER or a string designating a callback URL.
See INITIATE-PIN-AUTHENTICATION, INITIATE-SERVER-AUTHENTICATION and
INITIATE-REDIRECT-AUTHENTICATION respectively."
  (setf *oauth-consumer-key* consumer-key
        *oauth-consumer-secret* consumer-secret)
  (case method
    (:PIN (initiate-pin-authentication))
    (:SERVER (initiate-server-authentication))
    (T (initiate-redirect-authentication method))))

(defun access-token (verifier)
  "Turn the tokens received through the authentication into an access token.

According to spec https://dev.twitter.com/docs/auth/implementing-sign-twitter"
  (oauth-response->alist
   (signed-request *oauth/access-token* :parameters `(("oauth_verifier" . ,verifier)))))

(defun complete-authentication (verifier &optional (token *oauth-token*))
  "Finishes the authentication procedure by retrieving the access token.
Sets the *OAUTH-TOKEN* and *OAUTH-TOKEN-SECRET* to their respective values."
  (setf *oauth-token* token)
  (let ((data (access-token verifier)))
    (setf *oauth-token* (cdr (assoc :oauth-token data))
          *oauth-token-secret* (cdr (assoc :oauth-token-secret data)))
    (values *oauth-token* *oauth-token-secret*)))
