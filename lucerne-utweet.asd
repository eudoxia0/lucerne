(asdf:defsystem lucerne-utweet
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :defsystem-depends-on (:eco)
  :depends-on (:lucerne
               :crane
               :local-time
               :lucerne-auth
               :avatar-api)
  :components ((:module "examples/utweet"
                :serial t
                :components
                ((:file "models")
                 (:module "templates"
                  :serial t
                  :components
                  ((:eco-template "includes")
                   (:eco-template "tweets")
                   (:eco-template "pages")))
                 (:file "views"))))
  :description "A small Twitter clone built with Lucerne.")
