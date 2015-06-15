(asdf:defsystem lucerne-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:lucerne
               :lucerne-hello-world
               :lucerne-utweet
               :fiveam
               :drakma)
  :description "Lucerne tests"
  :components ((:module "t"
                :serial t
                :components
                ((:file "lucerne")
                 (:file "subapps")
                 (:file "examples")))))
