(asdf:defsystem lucerne-auth
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-pass
               :lucerne)
  :components ((:module "contrib/auth"
                :components
                ((:file "auth"))))
  :description "An authentication framework for Lucerne."
  :in-order-to ((test-op (test-op lucerne-auth-test))))
