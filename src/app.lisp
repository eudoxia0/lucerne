;;;; Here we define the class foundations of Lucerne. There are applications,
;;;; which have routes, middleware and sub-applications.
;;;;
;;;; Routes map a particular URL pattern and HTTP method to a function that
;;;; takes a request and returns a response to the client.
;;;;
;;;; Middleware is what we all know and love: An item of Clack middleware is a
;;;; class that wraps a request to perform some preprocessing, or regular
;;;; processing. For example, serving files, guarding against CSRF attacks,
;;;; etc.
;;;;
;;;; Sub applications give us the ability to compose different applications. For
;;;; example, you can have an `admin` application which implements an
;;;; administration panel, and has its own middleware (For serving the CSS and
;;;; JS used by the panel). You can integrate this application into your own
;;;; simply by adding it as a sub-application under a mount point (Most
;;;; typically "/admin/"). The ability to painlessly compose applications is
;;;; extremely valuable, as it allows you to easily modularize an application,
;;;; and even separate some functionality into a completely separate library.
;;;;
;;;; Lucerne applications are not strictly the same as Clack applications:
;;;; Rather than applying middleware and mounting sub-applications when an app
;;;; is defined, those things are kept in slots. When an application is started,
;;;; Lucerne recursively tranverses the tree of nested applications and
;;;; sub-applications, mounts them all together, and ensures all their
;;;; middleware is applied.
(in-package :lucerne)


(defclass <route> ()
  ((rule :initarg :rule
         :reader route-rule
         :type <url-rule>
         :documentation "Routing rule.")
   (method :initarg :method
           :reader route-method
           :type symbol
           :documentation "The HTTP method.")
   (fn :initarg :fn
       :reader route-function
       :type function
       :documentation "The function to call when the rule and method match."))
  (:documentation "Maps a path and HTTP method to a view function."))

(defclass <mount-point> ()
  ((sub-app :initarg :sub-app
            :reader sub-app
            :type <app>)))

(defclass <prefix-mount> (<mount-point>)
  ((prefix :initarg :prefix
           :reader prefix
           :type string)))

(defclass <app> (clack:<component>)
  ((routes :initarg :routing-rules
           :accessor app-routing-rules
           :initform nil
           :type (proper-list <route>)
           :documentation "The application's routes.")
   (builder-function :initarg :builder-function
                     :reader builder-function
                     :type function
                     :documentation "The function that wraps the app in
                     middleware and mounts sub-apps, returning a new app."))
  (:documentation "The base class for all Lucerne applications."))

(defmacro make-urlmap (&rest sub-apps)
  "Create a an app out of many sub-apps, where each sub-app is a `(<prefix>
<app>) pair."
  `(let ((urlmap (make-instance 'clack.app.urlmap:<clack-app-urlmap>)))
     ,@(loop for app in sub-apps collecting
         `(clack.app.urlmap:mount urlmap
                                  ,(first app)
                                  ,(second app)))
     urlmap))

(defmacro defapp (name &key middlewares sub-apps)
  "Define an application."
  `(defparameter ,name
     (make-instance
      'lucerne:<app>
      :builder-function
      (lambda (app)
        ;; First, wrap the app in its middlewares
        (let ((mw-wrapped
                (clack.builder:builder
                 ,@middlewares
                 app)))
          ;; Now, mount sub-apps
          ,(if sub-apps
               `(make-urlmap
                 ("/" mw-wrapped)
                 ,@sub-apps)
               `mw-wrapped))))))

(defmethod build-app ((app <app>))
  "Take a Lucerne application, and recursively mount sub-applications and apply
  middleware."
  (funcall (builder-function app) app))
