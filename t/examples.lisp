(in-package :cl-user)
(defpackage lucerne-test.examples
  (:use :cl :lucerne :fiveam)
  (:import-from :lucerne-test
                :+port+
                :make-url))
(in-package :lucerne-test.examples)

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

(test utweet
  ;; Create some test data
  (let ((john (utweet.models:register-user :username "john"
                                           :full-name "John Doe"
                                           :email "jdoe@initech.com"
                                           :password "pass"))
        (jane (utweet.models:register-user :username "jane"
                                           :full-name "Jane Doe"
                                           :email "j.doe@initech.com"
                                           :password "pass"))
        (user (utweet.models:register-user :username "user"
                                           :full-name "Test User"
                                           :email "test@example.com"
                                           :password "pass")))
    ;; Follow
    (utweet.models:follow user john)
    (utweet.models:follow user jane)
    ;; Add some tweets
    (utweet.models:tweet john "BEEP BOOP feed me followers")
    (utweet.models:tweet jane "boop beep i'm a test tweet")
    ;; Test the models are consistent
    (is
     (equal (hash-table-count utweet.models::*users*)
            3))
    (is
     (equal (length utweet.models::*subscriptions*)
            2))
    (is
     (equal (length utweet.models::*tweets*)
            2))
    ;; Bring up the app
    (lucerne:start utweet.views:app :port +port+)
    ;; Bring it down
    (lucerne:stop utweet.views:app)))

(run! 'examples)
