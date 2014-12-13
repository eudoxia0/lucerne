(in-package :cl-user)
(defpackage utweet.views
  (:use :cl :lucerne :crane :utweet.models)
  (:import-from :lucerne-auth
                :get-userid
                :login
                :logout
                :logged-in-p))
(in-package :utweet.views)
(annot:enable-annot-syntax)

;;; App definition

(defapp app
    :middlewares ((clack.middleware.static:<clack-middleware-static>
                   :path "/static/"
                   :root (asdf:system-relative-pathname :lucerne-utweet
                                                        #p"examples/utweet/static/"))))

;;; Views

(defun get-user (req)
  (let ((username (get-userid req)))
    (when username
      (single '<user> :username username))))

@route app "/"
(defview index ()
  (if (logged-in-p *request*)
      (render-template eco-template:timeline
                       (get-user *request*)
                       (user-timeline (get-user *request*)))
      (render-template eco-template:index)))

@route app "/profile/:username"
(defview profile (username)
  (let* ((user (single '<user> :username username))
         (avatar-url (avatar-url user))
         ;; The user's timeline
         (user-tweets (user-tweets user))
         ;; Is the user viewing his own profile?
         (is-self (equal (get-userid *request*) username)))
    (render-template eco-template:profile (get-userid *request*) user
                     avatar-url user-tweets is-self)))

@route app "/followers/:username"
(defview user-followers (username)
  (let ((user (single '<user> :username username)))
    (render-template eco-template:user-list (get-userid *request*) "Followers"
                     user (follower-summary user))))

@route app "/following/:username"
(defview user-following (username)
  (let ((user (single '<user> :username username)))
    (render-template eco-template:user-list (get-userid *request*) "Following"
                     user (following-summary user))))

;;; Authentication views

@route app (:post "/signup")
(defview sign-up ()
  (with-params *request* (name username email password password-repeat)
    (if (exists '<user> :username :username)
        ;; Does a user with that name exist? In that case, render the landing
        ;; template with a corresponding error
        (render-template eco-template:index "A user with that name already exists.")
        ;; We have a new user. Do both passwords match?
        (if (equal password password-repeat)
            ;; Okay, the passwords are a match. Let's create the user and return
            ;; the user to the homepage
            (progn
              (register-user username name email password)
              (redirect "/"))
            ;; The passwords don't match
            (render-template eco-template:index "Passwords don't match.")))))

@route app (:post "/signin")
(defview sign-in ()
  (with-params *request* (username password)
    ;; Check whether a user with this name exists
    (let ((user (single '<user> :username username)))
      (if user
          (if (cl-pass:check-password password (password user))
              (progn
                ;; Log the user in
                (login *request* username)
                (redirect "/"))
              ;; Wrong password
              (render-template eco-template:index "Wrong password."))
          ;; No such user
          (render-template eco-template:index "No such user.")))))

@route app (:get "/signout")
(defview sign-out ()
  (when (logged-in-p *request*)
    (logout *request*))
  (redirect "/"))

;;; Bring it up

(start app)
