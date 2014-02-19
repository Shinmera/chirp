#|
 This file is a part of Chirp
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.chirp.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.chirp.asdf)

(defsystem chirp
  :name "Chirp Twitter Client"
  :version "0.0.1"
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
               (:file "user")
               (:file "tweets"))
  :depends-on (:drakma
               :yason
               :split-sequence
               :uuid
               :ironclad
               :flexi-streams
               :local-time
               :cl-base64
               :cl-ppcre))

(defsystem chirp-doc
  :name "Chirp Doc"
  :components ((:file "documentation"))
  :depends-on (:chirp :lquery-doc))
