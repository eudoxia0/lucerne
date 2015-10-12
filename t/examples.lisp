(in-package :cl-user)
(defpackage lucerne-test.examples
  (:use :cl :lucerne :fiveam)
  (:import-from :lucerne-test
                :+port+
                :make-url)
  (:export :examples))
(in-package :lucerne-test.examples)

;;; Utilities

(defmacro response-ok (&rest arguments)
  `(multiple-value-bind (body status-code &rest other)
       (drakma:http-request ,@arguments)
     (declare (ignore body other))
     (is
      (equal status-code 200))))

;;; Tests

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
    (finishes
      (utweet.models:follow user john)
      (utweet.models:follow user jane))
    ;; Add some tweets
    (finishes
      (utweet.models:tweet john "BEEP BOOP feed me followers")
      (utweet.models:tweet jane "boop beep i'm a test tweet"))
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
    (finishes
      (lucerne:start utweet.views:app :port +port+))
    ;; Requests
    (is-true
     (search "Sign up"
             (drakma:http-request (make-url ""))))
    ;; Create an account
    (response-ok (make-url "signup")
                 :method :post
                 :parameters (list (cons "username" "eudoxia")
                                   (cons "name" "Fernando")
                                   (cons "email" "eudoxiahp@gmail.com")
                                   (cons "password" "pass")
                                   (cons "password-repeat" "pass")))
    (finishes
      (utweet.models:follow (utweet.models:find-user "eudoxia") john)
      (utweet.models:follow (utweet.models:find-user "eudoxia") jane))
    (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
      ;; Log in
      (response-ok (make-url "signin")
                   :method :post
                   :parameters (list (cons "username" "eudoxia")
                                     (cons "password" "pass"))
                   :cookie-jar cookie-jar)
      ;; View the timeline
      (response-ok (make-url "")
                   :cookie-jar cookie-jar)
      ;; View the profile
      (response-ok (make-url "profile/eudoxia")
                   :cookie-jar cookie-jar)
      ;; Followers and following
      (response-ok (make-url "followers/eudoxia")
                   :cookie-jar cookie-jar)
      (response-ok (make-url "following/eudoxia")
                   :cookie-jar cookie-jar)
      ;; Send a tweet
      (response-ok (make-url "tweet")
                   :method :post
                   :parameters (list (cons "tweet" "test"))
                   :cookie-jar cookie-jar)
      (is
       (equal (length utweet.models::*tweets*)
              3))
      ;; Log out
      (response-ok (make-url "signout")
                   :cookie-jar cookie-jar))
    ;; Bring it down
    (lucerne:stop utweet.views:app)))
