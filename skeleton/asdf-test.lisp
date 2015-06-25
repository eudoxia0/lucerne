(defsystem {{name}}-test
  :author "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
  {{#license}}:license "{{license}}"{{/license}}
  :description "Tests for {{name}}."
  :depends-on (:{{name}}
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "{{name}}")))))
