#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#



(asdf:defsystem chirp-dexador
  :version "0.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Chirp Twitter client using Dexador as backend."
  :serial T
  :components ((:file "dexador"))
  :depends-on (:chirp-core
               :dexador))
