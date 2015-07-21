(in-package :cl-user)
(defpackage {{name}}
  (:use :cl :lucerne)
  (:export :app)
  (:documentation "Main {{name}} code."))
(in-package :{{name}})

;;; App

(defapp app
  :middlewares ((clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :{{name}} #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :{{name}} #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))

;;; Views

@route app "/"
(defview index ()
  (render-template (+index+)))
