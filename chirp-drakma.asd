(asdf:defsystem chirp-drakma
  :version "0.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Chirp Twitter client using Drakma as backend."
  :serial T
  :components ((:file "drakma"))
  :depends-on (:chirp-core
               :drakma))
