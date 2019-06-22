#|
 This file is a part of Chirp
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#



(asdf:defsystem chirp
  :version "0.2.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Chirp Twitter client featuring full API coverage."
  :homepage "https://shinmera.github.io/chirp/"
  :bug-tracker "https://github.com/Shinmera/chirp/issues"
  :source-control (:git "https://github.com/Shinmera/chirp.git")
  :serial T
  :depends-on (:chirp-drakma))
