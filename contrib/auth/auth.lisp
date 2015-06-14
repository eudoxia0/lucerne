(in-package :cl-user)
(defpackage lucerne-auth
  (:use :cl)
  (:import-from :clack.request
                :env)
  (:export :get-userid
           :login
           :logout
           :logged-in-p))
(in-package :lucerne-auth)

(defclass auth-manager ()
  ((get-user :initarg :get-user
             :reader  get-user
             :type    function)
   (user-pass :initarg :user-pass
              :reader   user-pass
              :type     function)))

(defun get-userid ()
  (gethash :userid (lucerne:session lucerne:*request*)))

(defun login (userid)
  (setf (gethash :userid (lucerne:session lucerne:*request*))
        userid))

(defun logout ()
  (remhash :userid (get-session lucerne:*request*)))

(defun logged-in-p ()
  (if (get-userid lucerne:*request*) t))
