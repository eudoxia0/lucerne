(in-package :cl-user)
(defpackage lucerne.views
  (:use :cl :trivial-types :cl-annot :anaphora)
  (:import-from :clack.request
                :make-request
                :request-method
                :script-name
                :request-uri)
  (:export :not-found
           :define-route
           :defview
           :req
           :route))
(in-package :lucerne.views)

(defmethod not-found ((app lucerne.app:<app>) req)
  "The basic `not-found` screen: Returns HTTP 404 and the text 'Not found'."
  (declare (ignore req))
  (lucerne.http:respond "Not found" :type "text/plain" :status 404))

(defun strip-app-prefix (url app-prefix)
  (if (> (length app-prefix) 0)
      (subseq url (1- (length app-prefix)))
      url))

(defmethod clack:call ((app lucerne.app:<app>) env)
  "Routes the request determined by `env` on the application `app`."
  (let* ((req    (make-request env))
         (method (request-method req))
         (prefix (script-name req))
         (uri    (request-uri req))
         (final-uri (strip-app-prefix uri prefix))
         ;; Now, we actually do the dispatching
         (route (myway:dispatch (lucerne.app:routes app)
                                final-uri
                                :method method)))
    (if route
        ;; We have a hit
        (funcall route req)
        ;; Not found
        (not-found app req))))

(defmethod define-route ((app lucerne.app:<app>) url method fn)
  "Map `method` calls to `url` in `app` to the function `fn`."
  (myway:connect (lucerne.app:routes app)
                 url
                 (lambda (params)
                   ;; Dispatching returns a function that closes over `params`
                   (lambda (req)
                     (funcall fn params req)))
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
       (declare (ignore params))
       ,@body)))
