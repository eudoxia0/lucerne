(asdf:defsystem lucerne
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:clack
               :cl-annot
               :trivial-types
               :clack-errors
               :eco
               :anaphora)
  :components ((:module "src"
                :components
                ((:file "lucerne"))))
  :description "A Clack-based microframework."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lucerne-test))))
