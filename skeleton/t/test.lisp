(in-package :cl-user)
(defpackage {{name}}-test
  (:use :cl :fiveam))
(in-package :{{name}}-test)

(def-suite tests
  :description "{{name}} tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(run! 'tests)
