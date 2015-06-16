(in-package :cl-user)
(defpackage lucerne-hello-world
  (:use :cl :lucerne)
  (:export :app))
(in-package :lucerne-hello-world)
(annot:enable-annot-syntax)

(defapp app)

@route app "/"
(defview hello ()
  (respond "Hello, world!"))
