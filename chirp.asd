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
               (:file "oauth"))
  :depends-on (:drakma
               :yason
               :split-sequence))

(defsystem chirp-doc
  :name "Chirp Doc"
  :components ((:file "documentation"))
  :depends-on (:chirp :lquery-doc))
