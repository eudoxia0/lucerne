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

(defmacro with-params (req params &rest body)
  "Extract the parameters in `param` from the request `req`, and bind them for
use in `body`."
  `(let ,(mapcar #'(lambda (param)
                     `(,param (parameter ,req
                                         ,(intern (string-downcase
                                                   (symbol-name param))
                                                  :keyword))))
                 params)
     ,@body))

(defmacro render-template (template-name &rest args)
  "Render an Eco template `template-name` passing arguments `args`."
  `(respond (,template-name ,@args)))
