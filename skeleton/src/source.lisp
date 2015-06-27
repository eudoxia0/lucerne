(in-package :cl-user)
(defpackage {{name}}
  (:use :cl :lucerne)
  (:export :app)
  (:documentation "Main {{name}} code."))
(in-package :{{name}})

;;; App

(defapp app
  :middlewares ((<clack-middleware-static>
                 :root (asdf:system-relative-pathname :self #p"assets/build/")
                 :path "/static/")))

;;; Assets

(rock:defenv :{{name}}
  :assets ((:jquery :1.11.1))
  :bundles ((:js
            :assets ((:jquery :1.11.1))
            :files (list #p"js/scripts.js")
            :destination #p"js/scripts.js")
            (:css
             :assets ()
             :files (list #p"css/style.css")
             :destination #p"css/style.css")))

;;; Templates


;;; Views
