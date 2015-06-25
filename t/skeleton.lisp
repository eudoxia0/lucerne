(in-package :cl-user)
(defpackage lucerne-test.skeleton
  (:use :cl :fiveam))
(in-package :lucerne-test.skeleton)

(def-suite skeleton
  :description "Project skeleton tests.")
(in-suite skeleton)

(defparameter +directory+
  (asdf:system-relative-pathname :lucerne #p"t/"))

(defparameter +input+ (format nil "app
author
email
license
desc
a,b,c
yes
yes
yes
yes
github-user
~A
" +directory+))

(defparameter +files+
  (list #p"README.md"
        #p".gitignore"
        #p"app.asd"
        #p"app-test.asd"
        #p"src/app.lisp"
        #p"t/app.lisp"
        #p"docs/manifest.lisp"
        #p"docs/manual.scr"
        #p"assets/css/style.scss"
        #p"assets/js/scripts.js"))

(test generate
  (with-input-from-string (input-stream +input+)
    (let ((*query-io* (make-two-way-stream input-stream *standard-output*))
          (*standard-input* input-stream)
          (app-directory (merge-pathnames #p"app/"
                                          +directory+)))
      (finishes
        (lucerne.skeleton:make-project))
      (loop for file in +files+ do
        (is-true
         (probe-file (merge-pathnames file app-directory))))
      (when (probe-file app-directory)
        (uiop:delete-directory-tree app-directory :validate t)))))

(run! 'skeleton)
