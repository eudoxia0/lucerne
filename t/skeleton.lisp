(in-package :cl-user)
(defpackage lucerne-test.skeleton
  (:use :cl :fiveam)
  (:export :skeleton))
(in-package :lucerne-test.skeleton)

(def-suite skeleton
  :description "Project skeleton tests.")
(in-suite skeleton)

(defparameter +directory+
  (asdf:system-relative-pathname :lucerne #p"t/"))

(defparameter +input+ "app
author
email
license
desc
a,b,c
~A
yes
yes
yes
github-user
~A
")

(defparameter +files+
  (list #p"README.md"
        #p".gitignore"
        #p"app.asd"
        #p"app-test.asd"
        #p"src/app.lisp"
        #p"t/app.lisp"
        #p"templates/base.html"
        #p"templates/includes/head.html"
        #p"templates/index.html"
        #p"docs/manifest.lisp"
        #p"docs/manual.scr"
        #p"assets/js/scripts.js"))

(test generate
  (loop for sass-option in (list "yes" "no") do
    (let ((input (format nil +input+ sass-option +directory+)))
      (with-input-from-string (input-stream input)
        (let ((*query-io* (make-two-way-stream input-stream *standard-output*))
              (*standard-input* input-stream)
              (app-directory (merge-pathnames #p"app/"
                                              +directory+)))
          (finishes
           (lucerne.skeleton:make-project))
          (loop for file in +files+ do
            (is-true
             (probe-file (merge-pathnames file app-directory))))
          (is-true
           (if (string= sass-option "yes")
               (probe-file (merge-pathnames #p"assets/css/style.scss" app-directory))
               (probe-file (merge-pathnames #p"assets/css/style.css" app-directory))))
          (when (probe-file app-directory)
            (uiop:delete-directory-tree app-directory :validate t)))))))
