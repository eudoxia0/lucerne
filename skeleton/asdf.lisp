(defsystem {{name}}
  :author "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
  :maintainer "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
  {{#license}}:license "{{license}}"{{/license}}
  :version "0.1"
  {{#ghuser}}:homepage "https://github.com/{{ghuser}}/{{name}}"
  :bug-tracker "https://github.com/{{ghuser}}/{{name}}/issues"
  :source-control (:git "git@github.com:{{ghuser}}/{{name}}.git"){{/ghuser}}
  :depends-on ({{dependencies}})
  :components ((:module "src"
                :serial t
                :components
                ((:file "{{name}}"))))
  :description "{{description}}"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op {{name}}-test))))
