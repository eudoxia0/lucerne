(in-package :cl-user)
(defpackage lucerne.app
  (:use :cl :trivial-types :cl-annot)
  (:export :prefix-mount
           :prefix
           :base-app
           :<app>
           :routes
           :middlewares
           :sub-apps
           :handler
           :register
           :use
           :build-app
           :defapp)
  (:documentation "Here we define the class foundations of Lucerne. There are
applications, which have routes, middleware and sub-applications.

Routes map a particular URL pattern and HTTP method to a function that takes a
request and returns a response to the client.

Middleware is what we all know and love: An item of Clack middleware is a class
that wraps a request to perform some preprocessing, or regular processing. For
example, serving files, guarding against CSRF attacks, etc.

Sub applications give us the ability to compose different applications. For
example, you can have an `admin` application which implements an administration
panel, and has its own middleware (For serving the CSS and JS used by the
panel). You can integrate this application into your own simply by adding it as
a sub-application under a mount point (Most typically '/admin/'). The ability to
painlessly compose applications is extremely valuable, as it allows you to
easily modularize an application, and even separate some functionality into a
completely separate library.

Lucerne applications are not strictly the same as Clack applications: Rather
than applying middleware and mounting sub-applications when an app is defined,
those things are kept in slots. When an application is started, Lucerne
recursively tranverses the tree of nested applications and sub-applications,
mounts them all together, and ensures all their middleware is applied."))
(in-package :lucerne.app)

(defclass mount-point () ())

(defclass prefix-mount ()
  ((prefix :reader prefix
           :initarg :prefix
           :type string)
   (app :reader app
        :initarg :app
        :type base-app))
  (:documentation "Maps a prefix to a sub-application."))

(defclass base-app (clack:<component>)
  ((routes :accessor routes
           :initform (myway:make-mapper)
           :type myway.mapper:mapper
           :documentation "The application's routes.")
   (middlewares :accessor middlewares
                :initarg :middlewares
                :initform nil
                :type list
                :documentation "List of middlewares the application will run.")
   (sub-apps :accessor sub-apps
             :initarg :sub-apps
             :initform nil
             :type (proper-list mount-point)
             :documentation "A list of sub-application mount points.")
   (handler :accessor handler
            :initform nil
            :documentation "The server handler."))
  (:documentation "The base class for all Lucerne applications."))

(defmethod register ((app base-app) prefix (sub-app base-app))
  "Mount `sub-app` to `app` on the prefix `prefix`."
  (push (make-instance 'prefix-mount
                       :prefix prefix
                       :app sub-app)
        (sub-apps app)))

(defmethod use ((app base-app) middleware)
  "Make `app` use the middleware instance `middleware`."
  (push middleware (middlewares app)))

;;; Internals

(defun apply-middlewares-list (app middleware-list)
  "Apply the middlewares in `middleware-list` to `app`, returning a new app."
  (if middleware-list
      (clack:wrap (first middleware-list)
                  (apply-middlewares-list app (rest middleware-list)))
      app))

(defun apply-mounts (app)
  "Recursively go through an app, mounting sub-applications to their prefix URLs
and returning the resulting mounted app."
  (if (sub-apps app)
      (let ((resulting-app (make-instance 'clack.app.urlmap:<clack-app-urlmap>)))
        (clack.app.urlmap:mount resulting-app "/" app)
        (loop for mount-point in (sub-apps app) do
          (clack.app.urlmap:mount resulting-app
                                  (prefix mount-point)
                                  (build-app (app mount-point))))
        resulting-app)
      app))

(defmethod build-app ((app base-app))
  "Take a Lucerne application, and recursively mount sub-applications and apply
  middleware."
  (apply-middlewares-list (apply-mounts app) (middlewares app)))

;;; Application definition

(defmacro defapp (name &key middlewares sub-apps (class ''base-app))
  "Define an application."
  (alexandria:with-gensyms (app)
    `(defparameter ,name
       (let ((,app (make-instance ,class)))
         ;; Use the middlewares
         ,@(loop for mw in middlewares collecting
             (if (listp mw)
                 ;; The middleware is a list, so we splice in a make-instance
                 `(use ,app (make-instance ',(first mw) ,@(rest mw)))
                 ;; The middleware is just a class name with no arguments
                 `(use ,app (make-instance ',mw))))
         ;; Register the sub-applications
         ,@(loop for sub-app in sub-apps collecting
             `(register ,app ,(first sub-app) ,(second sub-app)))
         ,app))))
