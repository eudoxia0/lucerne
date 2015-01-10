(in-package :lucerne-test)
(annot:enable-annot-syntax)

(def-suite subapps
  :description "Application composition tests.")
(in-suite subapps)

;;; The structure of the apps looks like this
;;;
;;; / : parent
;;; /s1/
;;;   / : sub-app
;;;   /s/ : sub-sub-app
;;; /s2/
;;;   / : sub-app-2

(defapp sub-sub-app)

@route sub-sub-app "/"
(defview sub-sub-app-index ()
  (respond "sub sub app"))

(defapp subapp-1
  :sub-apps (("/s" sub-sub-app)))

(defapp subapp-2)

@route subapp-1 "/"
(defview subapp-1-index ()
  (respond "sub app 1"))

@route subapp-2 "/"
(defview subapp-2-index ()
  (respond "sub app 2"))

(defapp parent-app
  :sub-apps (("/s1" subapp-1)
             ("/s2" subapp-2)))

@route parent-app "/"
(defview parent-index ()
  (respond "main app"))

(test bring-up-subapps
  (is-true
   (start parent-app :port +port+)))

(test (sub-apps-work :depends-on bring-up-subapps)
  (is
   (equal "main app"
          (drakma:http-request (make-url ""))))
  (is
   (equal "sub app 1"
          (drakma:http-request (make-url "s1/"))))
  (is
   (equal "sub app 2"
          (drakma:http-request (make-url "s2/")))))

(test (bring-down-subapps :depends-on bring-up-subapps)
  (is-true
   (stop parent-app)))

(run! 'subapps)
