(in-package :cl-user)
(defpackage lucerne-test
  (:use :cl :lucerne :fiveam))
(in-package :lucerne-test)
(annot:enable-annot-syntax)

(defparameter +port+ 4545)

(defun make-url (rest)
  (concatenate 'string
               (format nil "http://localhost:~A/" +port+)
               rest))

(def-suite basic
  :description "Basic tests.")
(in-suite basic)

(defapp app)

(test define-routes
  (finishes
    @route app "/"
    (defview index ()
      (respond "<h1>Welcome to Lucerne</h1>")))
  (finishes
    @route app "/greet/:name"
    (defview greet (name)
      (respond (format nil "Hello, ~A!" name))))
  (finishes
    @route app "/add/:a/:b"
    (defview add-numbers (a b)
      (respond (format nil "~A" (+ (parse-integer a)
                                   (parse-integer b))))))
  (finishes
    @route app "/unicode"
    (defview unicode-test ()
      (respond "😸")))
  (finishes
    @route app "/redirect"
    (defview redirect-test ()
      (redirect (make-url ""))))
  (finishes
    @route app (:post "/post")
    (defview post-test ()
      (with-params *request* (a b)
        (respond (format nil "~A ~A" a b))))))

(test (bring-up :depends-on define-routes)
  (is-true
   ;; Starting the server for the first time
   (start app :port +port+))
  (is-false
   ;; Restarting the server
   (start app :port +port+)))

(test (views-work :depends-on bring-up)
  (is
   (equal "<h1>Welcome to Lucerne</h1>"
          (drakma:http-request (make-url ""))))
  (is
   (equal "Hello, eudoxia!"
          (drakma:http-request (make-url "greet/eudoxia"))))
  (is
   (equal "2"
          (drakma:http-request (make-url "add/1/1"))))
  (is
   (equal "😸"
          (drakma:http-request (make-url "unicode"))))
  (is-true
   (puri:uri= (puri:uri (make-url ""))
              (multiple-value-bind (body status params uri &rest others)
                  (drakma:http-request (make-url "redirect"))
                uri)))
  (is
   (equal "1 2"
          (drakma:http-request (make-url "post")
                               :method :post
                               :parameters '(("a" . "1")
                                             ("b" . "2")))))
  (is
   (equal "Not found"
          (drakma:http-request (make-url "no-such-view")))))

(test (bring-down :depends-on bring-up)
  (is-true
   ;; Stop the app
   (stop app))
  (is-false
   ;; Try to stop it again, should do nothing and return NIL
   (stop app)))

(run! 'basic)
