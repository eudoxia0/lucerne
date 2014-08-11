(in-package :cl-user)
(defpackage lucerne
  (:use :cl :trivial-types :cl-annot)
  (:import-from :clack.util.route
                :<url-rule>
                :match)
  (:import-from :clack.request
                :make-request
                :request-method
                :request-uri)
  (:export :<app>
           :not-found
           :respond))
(in-package :lucerne)
(annot:enable-annot-syntax)

;;; Internals

(defclass <route> ()
  ((rule :initarg :rule
         :reader route-rule
         :type <url-rule>)
   (method :initarg :method
           :reader route-method
           :type symbol)
   (fn :initarg :fn
       :reader route-function
       :type function))
  (:documentation "Maps a path and method to a view function."))

(defclass <app> (clack:<component>)
  ((routes :initarg :routing-rules
           :accessor app-routing-rules
           :initform nil
           :type (proper-list <route>)))
  (:documentation "The base class for all Lucerne applications."))

(defmethod not-found ((app <app>) req)
  "The basic `not-found` screen: Returns HTTP 404 and the text 'Not found'."
  (declare (ignore req))
  (respond "Not found" :type "text/plain" :status 404))

(defmethod clack:call ((app <app>) env)
  "Routes the request determined by `env` on the application `app`."
  (let* ((req    (make-request env))
         (method (request-method req))
         (uri    (request-uri req)))
    (loop for route in (app-routing-rules app) do
      (multiple-value-bind (url params)
          (match (route-rule route) method uri)
        (if url
            (return-from clack:call (funcall (route-function route)
                                             params
                                             req)))))
    (not-found app req)))

;;; Utilities

(defun respond (body &key (type "text/html;charset=utf-8") (status 200))
  "Construct a response from a `body`, content `type` and `status` code."
  (list status
        (list :content-type type)
        (list body)))
