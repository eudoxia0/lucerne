(in-package :cl-user)
(defpackage lucerne.skeleton
  (:use :cl)
  (:export :make-project)
  (:documentation "Lucerne project skeleton generator."))
(in-package :lucerne.skeleton)

;;; Utilities

(defparameter +skeleton-directory+
  (asdf:system-relative-pathname :lucerne #p"skeleton/"))

(defun skeleton-to-file (skeleton-file target-file directory plist-data)
  (let ((skeleton-pathname (merge-pathnames skeleton-file
                                            +skeleton-directory+))
        (target-pathname (merge-pathnames target-file
                                          directory)))
    (let ((target-directory (uiop:pathname-directory-pathname target-pathname)))
      (ensure-directories-exist target-directory))
    (with-open-file (output-stream target-pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (format t ";; Writing ~A~%" target-pathname)
      (mustache:render (uiop:read-file-string skeleton-pathname)
                       (alexandria:plist-alist plist-data)
                       output-stream))))

(defun strip-whitespace (string)
  (string-trim '(#\Space #\Tab) string))

(defun parse-systems-list (systems-list)
  (loop for system-name
        in (split-sequence:split-sequence #\, systems-list)
        collecting
        (strip-whitespace system-name)))

;;; Generate

(defun make-project ()
  "Generate a project by interactively asking questions."
  (flet ((ask (format-string &rest args)
           (let ((string (apply #'format (append (list nil format-string)
                                                 args))))
             (format t "~%~A: " string)
             (read-line)))
         (yes-or-no (format-string &rest args)
           (apply #'yes-or-no-p (cons format-string args))))
    (let* ((name (ask "Project name (e.g. 'my-app')"))
           (author (ask "Author's full name"))
           (email (ask "Author's email"))
           (license (ask "License (e.g. 'MIT', 'GPLv3')"))
           (description (ask "One-line project description"))
           (dependencies (parse-systems-list
                          (ask "Dependencies (e.g. 'drakma, quri, clack')")))
           (sassp (yes-or-no "Use Sass as the CSS preprocessor?"))
           (travisp (yes-or-no "Use Travis for continuous integration?"))
           (gitignorep (yes-or-no "Add .gitignore?"))
           (githubp (yes-or-no "Do you have a GitHub username?"))
           (github-user (if githubp
                            (ask "GitHub username")
                            nil))
           (parent-directory (uiop:ensure-directory-pathname
                              (parse-namestring
                               (ask "Finally, where do we put the project directory? (e.g. '/code/lisp/' will put the project in '/code/lisp/~A')"
                                    name))))
           (directory (merge-pathnames (make-pathname :directory (list :relative name))
                                       parent-directory))
           (plist (list :name name
                        :author author
                        :email email
                        :license license
                        :description description
                        ;; Format the deps for the sys. def. file
                        :dependencies (format nil "~{:~A~^~%               ~}"
                                              dependencies)
                        :sassp sassp
                        :travisp travisp
                        :ghuser github-user
                        ;; For the README
                        :year (write-to-string
                               (local-time:timestamp-year
                                (local-time:now))))))
      (flet ((generate (skeleton target &rest args)
               (apply #'skeleton-to-file (list skeleton
                                               target
                                               directory
                                               (append plist args)))))
        ;; Ensure directories exist
        (loop for dir in (list #p"src/" #p"t/" #p"docs/") do
          (ensure-directories-exist (merge-pathnames dir
                                                     directory)))
        ;; The README
        (generate #p"README.md" #p"README.md")
        ;; The .gitignore
        (when gitignorep
          (generate #p"gitignore.txt" #p".gitignore"))
        ;; Source code
        (generate #p"asdf.lisp" (parse-namestring
                                 (format nil "~A.asd" name)))
        (generate #p"src/source.lisp" (parse-namestring
                                       (format nil "src/~A.lisp" name)))
        ;; Tests
        (generate #p"asdf-test.lisp" (parse-namestring
                                      (format nil "~A-test.asd" name)))
        (generate #p"t/test.lisp" (parse-namestring
                                   (format nil "t/~A.lisp" name)))
        ;; Templates
        (generate #p"templates/base.html" #p"templates/base.html")
        (generate #p"templates/includes/head.html" #p"templates/includes/head.html")
        (generate #p"templates/index.html" #p"templates/index.html")
        ;; Assets
        (if sassp
            (generate #p"assets/css/style.scss" #p"assets/css/style.scss")
            (generate #p"assets/css/style.css" #p"assets/css/style.css"))
        (generate #p"assets/js/scripts.js" #p"assets/js/scripts.js")
        ;; Documentation
        (generate #p"docs/manifest.lisp" #p"docs/manifest.lisp")
        (generate #p"docs/manual.scr" #p"docs/manual.scr")
        t))))
