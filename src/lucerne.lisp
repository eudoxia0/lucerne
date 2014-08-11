(in-package :cl-user)
(defpackage lucerne
  (:use :cl :trivial-types :cl-annot)
  (:import-from :clack.util.route
                :<url-rule>)
  (:export :<app>))
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
