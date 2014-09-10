(in-package :cl-user)
(defpackage lucerne
  (:use :cl :trivial-types :cl-annot :anaphora)
  (:import-from :clack.util.route
                :<url-rule>
                :match
                :make-url-rule)
  (:import-from :clack.request
                :make-request
                :request-method
                :request-uri)
  (:export :<app>
           :not-found
           :add-route
           :defview
           :route
           :defapp
           :start
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

(defun add-route (app url method fn)
  "Add the route that maps `url` and `method` to `fn` to the application `app`."
  (push (make-instance '<route>
                       :rule (make-url-rule url :method method)
                       :method method
                       :fn fn)
        (app-routing-rules app)))

;;; Externals

;; A simple macro to define a view, because who is going to remember to type the
;; argument list?
(defmacro defview (name (&rest args) &rest body)
  "Define a view. The body of the view implicitly has access to the request
  object under the name `req`."
  `(defun ,(intern (symbol-name name)) (params req)
     ;; Here, we extract arguments from the params plist into the arguments
     ;; defined in the argument list
     (let ,(mapcar #'(lambda (arg)
                       `(,arg (getf params ,(intern (symbol-name arg)
                                                    :keyword))))
                   args)
       ,@body)))

;; Route annotation
(annot:defannotation route (app config body) (:arity 3)
  (let* ((view (second body)))
    (if (atom config)
        ;; The config is just a URL
        `(progn
           ,body
           (lucerne:add-route ,app
                              ,config
                              :get
                              #',view))
        ;; The config is a (<method> <url>) pair
        `(progn
           ,body
           (lucerne:add-route ,app
                              ,(second config)
                              ,(first config)
                              #',view)))))

;; Define an application
(defmacro defapp (name)
  "Define an application."
  `(defparameter ,name (make-instance 'lucerne:<app>)))

(defparameter *handlers*
  (make-hash-table)
  "Maps application names to their handlers.")

(defmacro start (app &key middleware (port 8000) debug error-function)
  "Bring up `app`, optionally using `middleware`. By default, `port` is 8000 and
`debug` is nil. If the server was not running, it returns `t`. If the server was
running, it restarts it and returns nil."
  `(let ((rebooted nil))
     (awhen (gethash ,app *handlers*)
       ;; The handler already exists, meaning the server is running. Bring it
       ;; down before bringing it up again.
       (setf rebooted t)
       (clack:stop it))
     (let ((handler
             (clack:clackup
              (clack.builder:builder
               (clack.middleware.session:<clack-middleware-session>
                :state
                (make-instance 'clack.session.state.cookie:<clack-session-state-cookie>))
               ,@(if debug
                     (if error-function
                         `((clack-errors:<clack-error-middleware>
                            :fn ,error-function))
                         `(clack-errors:<clack-error-middleware>)))
               ,@middleware
               ,app)
              :port ,port
              :server :hunchentoot)))
       (setf (gethash ,app *handlers*) handler)
       ;; If it was rebooted, return nil. Otherwise t.
       (not rebooted))))

;;; Utilities

(defun respond (body &key (type "text/html;charset=utf-8") (status 200))
  "Construct a response from a `body`, content `type` and `status` code."
  (list status
        (list :content-type type)
        (list body)))
