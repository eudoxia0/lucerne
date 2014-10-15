(asdf:defsystem lucerne-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:lucerne
               :fiveam
               :drakma)
  :components ((:module "t"
                :serial t
                :components
                ((:file "lucerne")
                 (:file "urlmap")
                 (:file "subapps")))))
