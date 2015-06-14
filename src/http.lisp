(in-package :cl-user)
(defpackage lucerne.http
  (:use :cl)
  (:import-from :clack.request
                :parameter
                :env)
  (:export :respond
           :redirect
           :session
           :with-params
           :render-template))
(in-package :lucerne.http)

(defun respond (body &key (type "text/html;charset=utf-8") (status 200))
  "Construct a response from a `body`, content `type` and `status` code."
  (list status
        (list :content-type type)
        (list body)))

(defun redirect (url &key (status 302))
  "Redirect a user to `url`, optionally specifying a status code `status` (302
by default)."
  (list status
        (list :location url)
        (list "")))

(defmacro session (req)
  "Extract the session hash table from a request.x"
  `(getf (env ,req) :clack.session))

(defmacro with-params (params &rest body)
  "Extract the parameters in `param` from the *request*, and bind them for use
in `body`."
  `(let ,(loop for param in params collecting
               `(,param (let ((str (parameter lucerne.views:*request*
                                              ,(intern (string-downcase
                                                        (symbol-name param))
                                                       :keyword))))
                          (if (equal str "")
                              nil
                              str))))
     ,@body))

(defmacro render-template (template-name &rest args)
  "Render an Eco template `template-name` passing arguments `args`."
  `(respond (,template-name ,@args)))
