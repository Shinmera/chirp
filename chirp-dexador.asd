(asdf:defsystem chirp-dexador
  :version "0.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Chirp Twitter client using Dexador as backend."
  :serial T
  :components ((:file "dexador"))
  :depends-on (:chirp-core
               :dexador))
