#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.chirp.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.chirp.asdf)

(defsystem chirp
  :name "Chirp Twitter Client"
  :version "0.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Yet another twitter client trying to support the full API."
  :serial T
  :components ((:file "package")
               (:file "indent")
               (:file "toolkit")
               (:file "oauth")
               (:file "location")
               (:file "trends")
               (:file "entities")
               (:file "help")
               (:file "cursor")
               (:file "user")
               (:file "account")
               (:file "blocks")
               (:file "suggestions")
               (:file "statuses")
               (:file "timelines")
               (:file "direct-messages")
               (:file "friends")
               (:file "favorites")
               (:file "saved-searches")
               (:file "lists")
               (:file "stream")
               (:file "generics"))
  :depends-on (:drakma
               :yason
               :split-sequence
               :uuid
               :ironclad
               :flexi-streams
               :local-time
               :cl-base64
               :cl-ppcre
               :alexandria))

;; (defsystem chirp-doc
;;   :name "Chirp Doc"
;;   :pathname #.(asdf:system-source-directory :chirp)
;;   :components ((:file "documentation"))
;;   :depends-on (:chirp :lquery-doc))
