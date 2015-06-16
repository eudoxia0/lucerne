(in-package :cl-user)
(defpackage utweet.views
  (:use :cl :lucerne)
  (:export :app))
(in-package :utweet.views)
(annot:enable-annot-syntax)

;;; App definition

(defapp app
  :middlewares (clack.middleware.session:<clack-middleware-session>
                (clack.middleware.static:<clack-middleware-static>
                 :path "/static/"
                 :root (asdf:system-relative-pathname :lucerne-utweet
                                                      #p"examples/utweet/static/"))))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :lucerne-utweet #p"examples/utweet/templates/"))

(defparameter +timeline+ (djula:compile-template* "timeline.html"))

(defparameter +index+ (djula:compile-template* "index.html"))

(defparameter +profile+ (djula:compile-template* "profile.html"))

(defparameter +user-list+ (djula:compile-template* "user-list.html"))

;;; Utilities

(defun current-user ()
  "Find the user from request data."
  (let ((username (lucerne-auth:get-userid)))
    (when username
      (utweet.models:find-user username))))

;;; Views

@route app "/"
(defview index ()
  (if (lucerne-auth:logged-in-p)
      ;; Serve the user's timeline
      (let ((user (current-user)))
        (render-template (+timeline+)
                         :username (utweet.models:user-username user)
                         :name (utweet.models:user-full-name user)
                         :timeline (utweet.models:user-timeline user)))
      (render-template (+index+))))


@route app "/profile/:username"
(defview profile (username)
  (let* ((user (utweet.models:find-user username))
         ;; The user's timeline
         (user-tweets (utweet.models:user-tweets user))
         ;; Is the user viewing his own profile?
         (is-self (string= (lucerne-auth:get-userid)
                           username)))
    (render-template (+profile+)
                     :user user
                     :user-tweets user-tweets
                     :is-self is-self)))


@route app "/followers/:username"
(defview user-followers (username)
  (let ((user (utweet.models:find-user username)))
    (render-template (+user-list+)
                     :user user
                     :title "Followers"
                     :users (utweet.models:followers user))))

@route app "/following/:username"
(defview user-following (username)
  (let ((user (utweet.models:find-user username)))
    (render-template (+user-list+)
                     :user user
                     :title "Following"
                     :users (utweet.models:following user))))


;;; Authentication views

@route app (:post "/signup")
(defview sign-up ()
  (with-params (name username email password password-repeat)
    ;; Does a user with that name exist?
    (if (utweet.models:find-user username)
        ;; If it does, render the landing template with a corresponding error
        (render-template (+index+)
                         :error "A user with that name already exists.")
        ;; We have a new user. Do both passwords match?
        (if (string= password password-repeat)
            ;; Okay, the passwords are a match. Let's create the user and return
            ;; the user to the homepage
            (progn
              (utweet.models:register-user :username username
                                           :full-name name
                                           :email email
                                           :password password)
              (redirect "/"))
            ;; The passwords don't match
            (render-template (+index+)
                             :error "Passwords don't match.")))))

@route app (:post "/signin")
(defview sign-in ()
  (with-params (username password)
    ;; Check whether a user with this name exists
    (let ((user (utweet.models:find-user username)))
      (if user
          (if (cl-pass:check-password password
                                      (utweet.models:user-password user))
              (progn
                ;; Log the user in
                (lucerne-auth:login username)
                (redirect "/"))
              ;; Wrong password
              (render-template (+index+)
                               :error "Wrong password."))
          ;; No such user
          (render-template (+index+)
                           :error "No such user.")))))

@route app "/signout"
(defview sign-out ()
  (when (lucerne-auth:logged-in-p)
    (lucerne-auth:logout))
  (redirect "/"))
