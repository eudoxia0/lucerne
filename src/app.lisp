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
(annot:enable-annot-syntax)

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
   (middlewares :initarg :middlewares
                :accessor middlewares
                :initform nil
                :type list
                :documentation "List of middlewares the application will run.")
   (sub-apps :initarg :sub-apps
             :accessor sub-apps
             :initform nil
             :type (list-of <mount-point>)
             :documentation "A list of sub-application mount points."))
  (:documentation "The base class for all Lucerne applications."))

(defmacro defapp (name &key middleware sub-apps)
  "Define an application."
  `(defparameter ,name (make-instance 'lucerne:<app>
                                      :middleware ,middleware
                                      :sub-apps ,sub-apps)))

(defmethod get-middlewares ((app <app>))
  "Recursively search an application and its sub-applications, extracting its
  middleware list."
  (append (middlewares app)
          (reduce #'append
                  (loop for sub-app in (sub-apps app) collecting
                    (get-middlewares app)))))

(defmethod apply-middlewares ((app <app>) middleware-list)
  "Wrap the application in middlewares."
  (if middleware-list
      (clack:wrap (first middleware-list)
                  (apply-middlewares app (rest middleware-list)))
      app))

(defmethod apply-mounts ((app <app>))
  "Recursively go through an app, mounting sub-applications to their prefix URLs
and returning the resulting mounted app."
  (if (sub-apps app)
      (let ((resulting-app (make-instance 'clack.app.urlmap:<clack-app-urlmap>)))
        (loop for mount-point in (sub-apps app) do
          (clack.app.urlmap:mount resulting-app
                                  (prefix mount-point)
                                  (apply-mounts (app mount-point))))
        resulting-app)
      app))

(defmethod build-app ((app <app>))
  "Take a Lucerne application, and recursively mount sub-applications and apply
  middleware."
  ;; We apply middlewares first, because if we applied the mounts first, we'd
  ;; might lose the middlewares slot in the app instances.
  (apply-mounts (apply-middlewares app)))
