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
  "Extract the user ID from the current session."
  (gethash :userid (lucerne:session)))

(defun login (userid)
  "Log in the user specified by @cl:param(userid)."
  (setf (gethash :userid (lucerne:session))
        userid))

(defun logout ()
  "Log out the current user."
  (remhash :userid (lucerne:session)))

(defun logged-in-p ()
  "Whether the user is logged in or not."
  (if (get-userid)
      t))
