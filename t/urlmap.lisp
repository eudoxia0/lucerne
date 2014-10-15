(in-package :lucerne-test)

(def-suite clack
  :description "Test Clack application composition.")
(in-suite clack)

(defun make-app (text)
  (lambda (env)
    `(200 (:content-type "text/plain") (,text))))

(defparameter *urlmap*
  (lucerne::make-urlmap
   ("/" (make-app "main app"))
   ("/s1/" (lucerne::make-urlmap
            ("/" (make-app "sub app 1"))
            ("/s/" (lucerne::make-urlmap
                    ("/" (make-app "sub sub app"))
                    ("/test" (make-app "test"))))))
   ("/s2/" (make-app "sub app 2"))))

(defparameter *clack-handler* nil)

(test bring-up
  (finishes
   (setf *clack-handler* (clack:clackup *urlmap* :port 3000))))

(defun test-view (url string)
  (equal string
         (drakma:http-request (concatenate 'string "http://localhost:3000" url))))

(test views-work
  (is-true (test-view "/" "main app"))
  (is-true (test-view "/s1/" "sub app 1"))
  (is-true (test-view "/s1/s/" "sub sub app"))
  (is-true (test-view "/s1/s/test" "test"))
  (is-true (test-view "/s2/" "sub app 2")))

(test (bring-down :depends-on bring-up)
  (finishes
    (clack:stop *clack-handler*)))

(run! 'clack)
