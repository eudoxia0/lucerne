(in-package :cl-user)
(defpackage utweet.models
  (:use :cl)
  ;; Functions
  (:export :register-user
           :followers
           :following
           :follower-summary
           :following-summary
           :tweet
           :user-timeline
           :user-tweets
           :follow))
(in-package :utweet.models)

;;; Models

(defclass user ()
  ((full-name :accessor user-full-name
              :initarg :full-name
              :type string)
   (email :accessor user-email
          :initarg :email
          :type string)
   (password :accessor user-password
             :initarg :password
             :type string)
   (avatar-url :accessor user-avatar-url
               :initarg :avatar-url
               :type string)))

(defclass subscription ()
  ((follower :reader subscription-follower
             :initarg :follower
             :type string
             :documentation "The follower's email.")
   (followed :reader subscription-followed
             :initarg :followed
             :type string
             :documentation "The followed's email.")))

(defclass tweet ()
  ((author :reader tweet-author
           :initarg :author
           :type string
           :documentation "The author's email.")
   (text :reader tweet-text
         :initarg :text
         :type string)
   (timestamp :reader tweet-timestamp
              :initarg :timestamp
              :initform (local-time:now))))

#|
(defun register-user (username full-name email password)
  "Register the user. Passwords are hashed."
  (create '<user> :username username
                  :full-name full-name
                  :email email
                  :password (cl-pass:hash password)
                  ;; Get the user's Gravatar URL in 120px
                  :avatar-url (avatar-api:gravatar email 120)))

(defun followers (user)
  "List of IDs of users that follow `user`."
  (loop for sub in (filter '<subscription> :target (id user)) collecting
    (follower sub)))

(defun following (user)
  "List of IDs of users followed by `user`."
  (loop for sub in (filter '<subscription> :follower (id user)) collecting
    (target sub)))

(defun follower-summary (user)
  "Username, full name, and avatar URL of `user`'s followers."
  (query (select (:username :full-name :avatar-url)
           (from '<user>)
           (where (:in :id (followers user))))))

(defun following-summary (user)
  "Username, full name, and avatar URL of the users that follow `user`."
  (query (select (:username :full-name :avatar-url)
           (from '<user>)
           (where (:in :id (following user))))))

(defun tweet (author text)
  (create '<tweet> :author (id author) :text text :date (local-time:now)))

(defun user-timeline (user)
  "Find the tweets for this user's timeline."
  (let* ((following-ids (following user))
         ;; Here we use the SQL DSL instead of Crane's (intentionally) simple and
         ;; limited interface.
         (tweets (query (select :*
                          (from '<tweet>)
                          (where (:in :author following-ids))))))
    (loop for tweet-plist in tweets collecting
      (plist->object '<tweet> tweet-plist))))

(defun user-tweets (user)
  "Return a user's tweets, sorted through time"
  (filter '<tweet> :author (id user)))

(defun follow (follower target)
  (create '<subscription> :follower (id follower) :target (id target)))

;;; Create some example data

(let ((eudox (register-user "eudoxia" "eudoxia" "black.linen99@gmail.com" "pass"))
      (john  (register-user "john" "John Doe" "jdoe@initech.com" "test"))
      (jane  (register-user "jane" "Jane Doe" "jane.doe@initech.com" "test")))
  ;; Make eudoxia follow both
  (follow eudox john)
  (follow eudox jane)
  ;; Now write some test tweets
  (tweet john "Test message BEEP")
  (tweet jane "Test message BOOP")
  (tweet john "BEEP BOOP feed me followers"))
|#
