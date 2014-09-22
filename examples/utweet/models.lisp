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
           :text))
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
