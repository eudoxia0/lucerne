(asdf:defsystem lucerne
  :version "0.2"
  :author "Fernando Borretti"
  :license "MIT"
  :homepage "https://github.com/eudoxia0/lucerne"
  :depends-on (:clack
               :myway
               :cl-annot
               :trivial-types
               :clack-errors
               :eco
               :anaphora
               :log4cl)
  :components ((:module "src"
                :components
                ((:file "app")
                 (:file "http")
                 (:file "views")
                 (:file "control")
                 (:file "lucerne"))))
  :description "A Clack-based microframework."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lucerne-test))))
