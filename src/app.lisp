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
