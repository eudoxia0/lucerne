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
      (with-params req (a b)
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

(defapp sub-sub-app)

@route sub-sub-app "/"
(defview sub-sub-app-index ()
  (respond "sub-sub app"))

(defapp subapp-1
  :sub-apps (("/subsub" sub-sub-app)))

(defapp subapp-2)

(defapp subapp-3)

@route subapp-1 "/test"
(defview subapp-1-index ()
  (respond "subapp 1"))

@route subapp-2 "/"
(defview subapp-2-index ()
  (respond "subapp 2"))

@route subapp-3 "/"
(defview subapp-3-index ()
  (respond "subapp 3"))

(defapp parent-app
  :sub-apps (("/sub1" subapp-1)
             ("/sub2" subapp-2)
             ("/sub3" subapp-3)))

@route parent-app "/test"
(defview parent-index ()
  (respond "parent app"))

(test (bring-up-subapps :depends-on views-work)
  (is-true
   (start parent-app :port +port+)))

(test (sub-apps-work :depends-on bring-up-subapps)
  (is
   (equal "parent app"
          (drakma:http-request (make-url "test"))))
  (is
   (equal "subapp 1"
          (drakma:http-request (make-url "sub1/test"))))
  (is
   (equal "subapp 2"
          (drakma:http-request (make-url "sub2/"))))
  (is
   (equal "subapp 3"
          (drakma:http-request (make-url "sub3/"))))
  (is
   (equal "sub-sub app"
          (drakma:http-request (make-url "sub1/subsub/")))))

(test (bring-down-subapps :depends-on bring-up-subapps)
  (is-true
   (stop parent-app)))

(run! 'basic)
