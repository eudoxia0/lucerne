(asdf:defsystem lucerne-utweet
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:lucerne
               :local-time
               :lucerne-auth
               :avatar-api)
  :components ((:module "examples/utweet"
                :serial t
                :components
                ((:file "models")
                 (:file "views"))))
  :description "A small Twitter clone built with Lucerne.")
