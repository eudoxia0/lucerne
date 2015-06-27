(in-package :cl-user)
(defpackage utweet.models
  (:use :cl)
  ;; Users
  (:export :user
           :user-username
           :user-full-name
           :user-password
           :user-avatar-url)
  ;; Subscriptions (follows)
  (:export :subscription
           :subscription-follower
           :subscription-followed)
  ;; Tweets
  (:export :tweet
           :tweet-author
           :tweet-text
           :tweet-timestamp)
  ;; Some functions
  (:export :find-user
           :register-user
           :followers
           :following
           :tweet
           :user-timeline
           :user-tweets
           :follow))
(in-package :utweet.models)

;;; Models

(defclass user ()
  ((username :accessor user-username
             :initarg :username
             :type string)
   (full-name :accessor user-full-name
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
               :type string))
  (:documentation "A user."))

(defclass subscription ()
  ((follower :reader subscription-follower
             :initarg :follower
             :type string
             :documentation "The follower's username.")
   (followed :reader subscription-followed
             :initarg :followed
             :type string
             :documentation "The followed's username."))
  (:documentation "Represents a user following another."))

(defclass tweet ()
  ((author :reader tweet-author
           :initarg :author
           :type string
           :documentation "The author's username.")
   (text :reader tweet-text
         :initarg :text
         :type string)
   (timestamp :reader tweet-timestamp
              :initarg :timestamp
              :initform (local-time:now)))
  (:documentation "A tweet."))

;;; Storage

(defparameter *users* (make-hash-table :test #'equal))

(defparameter *subscriptions* (list))

(defparameter *tweets* (list))

;;; Functions

(defun find-user (username)
  "Find a user by @cl:param(username), returns @c(NIL) if none is found."
  (gethash username *users*))

(defun register-user (&key username full-name email password)
  "Create a new user and hash their @cl:param(password)."
  (setf (gethash username *users*)
        (make-instance 'user
                       :username username
                       :full-name full-name
                       :email email
                       :password (cl-pass:hash password)
                       :avatar-url (avatar-api:gravatar email 120))))

(defun followers (user)
  "List of users (@c(user) instances) that follow @cl:param(user)."
  (mapcar #'(lambda (sub)
              (find-user (subscription-follower sub)))
          (remove-if-not #'(lambda (sub)
                             (string= (subscription-followed sub)
                                      (user-username user)))
                         *subscriptions*)))

(defun following (user)
  "List of users (@c(user) instances) the @cl:param(user) follows."
  (mapcar #'(lambda (sub)
              (find-user (subscription-followed sub)))
          (remove-if-not #'(lambda (sub)
                             (string= (subscription-follower sub)
                                      (user-username user)))
                         *subscriptions*)))

(defun tweet (author text)
  "Create a new tweet from @cl:param(author) containing @cl:param(text)."
  (push (make-instance 'tweet
                       :author (user-username author)
                       :text text)
        *tweets*))

(defun sort-tweets (tweets)
  "Given a list of tweets, sort them so the newest are first."
  (sort tweets
        #'(lambda (tweet-a tweet-b)
            (local-time:timestamp>= (tweet-timestamp tweet-a)
                                    (tweet-timestamp tweet-b)))))

(defun user-timeline (user)
  "Find the tweets for this @cl:param(user)'s timeline."
  (sort-tweets (remove-if-not #'(lambda (tweet)
                                  (or (member (tweet-author tweet)
                                              (following user)
                                              :test #'equal)
                                      (string= (tweet-author tweet)
                                               (user-username user))))
                              *tweets*)))

(defun user-tweets (user)
  "Return a @cl:param(user)'s tweets, sorted through time."
  (sort-tweets (remove-if-not #'(lambda (tweet)
                                  (string= (tweet-author tweet)
                                           (user-username user)))
                              *tweets*)))

(defun follow (follower followed)
  "Follow a user. Takes two @c(user) instances: @cl:param(follower) and @cl:param(followed)."
  (push (make-instance 'subscription
                       :follower (user-username follower)
                       :followed (user-username followed))
        *subscriptions*))
