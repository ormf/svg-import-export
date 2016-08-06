;;;; svg-export.asd

(asdf:defsystem #:svg-export
  :serial t
  :depends-on (#:cxml-stp
               #:orm-utils
               #:cl-ppcre
               #:cl-FAD
               #:csound-export
               #:cm)
  :components ((:file "package")
               (:file "svg-macros")
               (:file "svg-export" :depends-on ("svg-macros"))
	       (:file "svg-export-utils")
               (:file "svg-import")
               (:file "svg-operations")))

