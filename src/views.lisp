(in-package :cl-user)
(defpackage lucerne.views
  (:use :cl :trivial-types :cl-annot)
  (:import-from :clack.request
                :make-request
                :request-method
                :script-name
                :request-uri)
  (:export :not-found
           :define-route
           :defview
           :route))
(in-package :lucerne.views)

(defmethod not-found ((app lucerne.app:base-app))
  "The basic not found screen: Returns HTTP 404 and the text 'Not found'."
  (lucerne.http:respond "Not found" :type "text/plain" :status 404))

(defun strip-app-prefix-and-query-string (url app-prefix)
  (subseq url
          (max 0 (1- (length app-prefix)))
          (position #\? url)))

(defmethod clack:call ((app lucerne.app:base-app) env)
  "Routes the request determined by @cl:param(env) on the application
@cl:param(app)."
  (let* ((req    (make-request env))
         (method (request-method req))
         (prefix (script-name req))
         (uri    (request-uri req))
         (final-uri (strip-app-prefix-and-query-string uri prefix))
         ;; Now, we actually do the dispatching
         (route (myway:dispatch (lucerne.app:routes app)
                                final-uri
                                :method method)))
    (if route
        ;; We have a hit
        (funcall route req)
        ;; Not found
        (let ((lucerne.http:*request* req))
          (not-found app)))))

(defmethod define-route ((app lucerne.app:base-app) url method fn)
  "Map @cl:param(method) calls to @cl:param(url) in @cl:param(app) to the
function @cl:param(fn)."
  (myway:connect (lucerne.app:routes app)
                 url
                 (lambda (params)
                   ;; Dispatching returns a function that closes over `params`
                   (lambda (req)
                     (let ((lucerne.http:*request* req))
                       (funcall fn params))))
                 :method method))

(annot:defannotation route (app config body) (:arity 3)
  (if (atom config)
      ;; The config is just a URL
      `(progn
         (lucerne.views:define-route ,app
           ,config
           :get
           ,body))
      ;; The config is a (<method> <url>) pair
      `(progn
         (lucerne.views:define-route ,app
             ,(second config)
           ,(first config)
           ,body))))

(defmacro defview (name (&rest args) &body body)
  "Define a view. The body of the view implicitly has access to the global
request object @c(*request*)."
  (alexandria:with-gensyms (params)
    `(defun ,(intern (symbol-name name)) (,params)
       ,(unless args
          `(declare (ignore ,params)))
       ;; Here, we extract arguments from the params plist into the arguments
       ;; defined in the argument list
       (let ,(mapcar #'(lambda (arg)
                         `(,arg (getf ,params ,(intern (symbol-name arg)
                                                       :keyword))))
                     args)
         ,@body))))
