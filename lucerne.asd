(asdf:defsystem lucerne
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.2"
  :homepage "https://github.com/eudoxia0/lucerne"
  :bug-tracker "https://github.com/eudoxia0/lucerne/issues"
  :source-control (:git "git@github.com:eudoxia0/lucerne.git")
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
