(in-package :cl-user)
(defpackage lucerne-test
  (:use :cl :lucerne :fiveam))
(in-package :lucerne-test)
(annot:enable-annot-syntax)

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
      (redirect "http://example.com/")))
  (finishes
    @route app (:post "/post")
    (defview post-test ()
      (with-params req (a b)
        (respond (format nil "~A ~A" a b))))))

(run! 'basic)
