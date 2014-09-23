(in-package :cl-user)
(defpackage utweet.models
  (:use :cl :crane)
  (:export ;; Class names and slots
           :id
           :<user>
           :username
           :full-name
           :email
           :password
           :avatar-url
           :<subscription>
           :follower
           :target
           :<tweet>
           :author
           :date
           :text
           ;; Utility functions
           :register
           :followers
           :following
           :follower-summary
           :following-summary
           :tweet
           :user-timeline
           :user-tweets
           :follow))
(in-package :utweet.models)
(annot:enable-annot-syntax)

;;; Crane setup

(setup
 :migrations-directory
 (asdf:system-relative-pathname :lucerne-utweet #p"examples/utweet/migrations/")
 :databases
 '(:main
   (:type :sqlite3
    :name ":memory:")))

;; Since this is just an example app, we force Crane to create everything on
;; every run by deleting the migrations
(crane:delete-migrations t)

(connect)

;;; A quick extension to Crane: Handling timestamps

(definflate (stamp 'timestamp)
  ;; Inflate a database timestamp into a timestamp object
  (local-time:parse-timestring stamp))

(defdeflate (stamp local-time:timestamp)
  ;; Deflate a timestamp object into a string
  (local-time:format-timestring nil stamp))

;;; Models

(deftable <user> ()
  (username  :type text :uniquep t)
  (full-name :type text)
  (email     :type text)
  (password  :type text)
  (avatar-url :type text)
  (:documentation "The user object. The `password` field contains a digest of
  the password."))

(deftable <subscription> ()
  (follower :type integer :foreign (<user>))
  (target   :type integer :foreign (<user>))
  (:documentation "Represents a user following another."))

(deftable <tweet> ()
  (author :type integer :foreign (<user>))
  (text :type text)
  (date :type timestamp)
  (:documentation "A tweet."))

;;; Some utility functions

(defun register (username full-name email password)
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

(let ((eudox (register "eudoxia" "eudoxia" "black.linen99@gmail.com" "pass"))
      (john  (register "john" "John Doe" "jdoe@initech.com" "test"))
      (jane  (register "jane" "Jane Doe" "jane.doe@initech.com" "test")))
  ;; Make eudoxia follow both
  (follow eudox john)
  (follow eudox jane)
  ;; Now write some test tweets
  (tweet john "Test message BEEP")
  (tweet jane "Test message BOOP")
  (tweet john "BEEP BOOP feed me followers"))
