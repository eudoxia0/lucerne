(in-package :lucerne-test)

(def-suite examples
  :description "Test Lucerne examples.")
(in-suite examples)

(test hello-world
  (finishes
    (lucerne:start lucerne-hello-world:app :port +port+))
  (is (equal (drakma:http-request (make-url ""))
             "Hello, world!"))
  (finishes
    (lucerne:stop lucerne-hello-world:app)))

;(run! 'examples)
