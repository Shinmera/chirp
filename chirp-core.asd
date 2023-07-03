(asdf:defsystem chirp-core
  :version "0.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Core component of the chirp twitter client, excluding the backend."
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
               (:file "media")
               (:file "statuses")
               (:file "timelines")
               (:file "direct-messages")
               (:file "friends")
               (:file "favorites")
               (:file "saved-searches")
               (:file "search")
               (:file "lists")
               (:file "stream")
               (:file "generics")
               (:file "collections"))
  :depends-on (:yason
               :split-sequence
               :uuid
               :ironclad
               :flexi-streams
               :local-time
               :cl-base64
               :cl-ppcre
               :alexandria
               :babel))
