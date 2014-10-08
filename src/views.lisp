(in-package :lucerne)
(annot:enable-annot-syntax)

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
