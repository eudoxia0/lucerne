(asdf:defsystem lucerne-hello-world
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:lucerne)
  :components ((:module "examples/hello-world"
                :components
                ((:file "app"))))
  :description "The simplest Lucerne app.")
