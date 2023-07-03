(defpackage #:org.tymoonnext.chirp.doc
  (:use #:cl #:lquery #:lquery-doc)
  (:nicknames #:chirp-doc)
  (:export #:build-documentation))

(in-package #:org.tymoonnext.chirp.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object))))
  ($ template "h3 a" (attr :href (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :chirp))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :chirp-api :exclude '(:internal :method))))
      ($ "#docs-api" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :chirp-extra :exclude '(:internal :method))))
      ($ "#docs-extra" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :chirp-objects :exclude '(:internal :method))))
      ($ "#docs-objects" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :chirp)))))

