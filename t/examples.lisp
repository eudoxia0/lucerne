(in-package :cl-user)
(defpackage lucerne-test.examples
  (:use :cl :fiveam))
(in-package :lucerne-test.examples)

(def-suite examples
  :description "Test Lucerne examples.")
(in-suite examples)

(test hello-world
  (finishes
    (lucerne:start lucerne-hello-world:app :port 8000))
  (is (equal (drakma:http-request "http://localhost:8000/")
             "Hello, world!"))
  (finishes
    (lucerne:stop lucerne-hello-world:app)))

(run! 'examples)
