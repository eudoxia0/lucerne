(in-package :cl-user)
(defpackage lucerne.ctl
  (:use :cl)
  (:import-from :anaphora
                :awhen
                :it)
  (:import-from :lucerne.app
                :<app>
                :running-port
                :build-app)
  (:export :start
           :stop)
  (:documentation "Lucerne keeps a database of running applications and their
port numbers. This is useful when loading a system that starts a Lucerne
application, but either does not shut it down or fails to do so because of an
error -- This is particularly common in testing. By using this system, we don't
leak ports and prevent 'address in use' errors."))
(in-package :lucerne.ctl)

(defparameter *handlers*
  (make-hash-table)
  "A map of ports to server handlers.")

(defmethod start ((app <app>) &key (port 8000))
  "Bring up `app`, by default on `port` 8000. If the server was not running, it
returns `t`. If the server was running, it restarts it and returns nil."
  (let ((rebooted nil))
    (awhen (gethash (running-port app) *handlers*)
      ;; The handler already exists, meaning the server is running. Bring it
      ;; down before bringing it up again.
      (setf rebooted t)
      (clack:stop it))
    (let ((handler
            (clack:clackup
             (lack:builder (build-app app))
             :port port
             :server :hunchentoot
             :use-default-middlewares nil)))
      (setf (running-port app) port)
      (setf (gethash port *handlers*) handler)
      ;; If it was rebooted, return nil. Otherwise t.
      (not rebooted))))

(defmethod stop ((app <app>))
  "If `app` is running, stop it and return T. Otherwise, do nothing and
return NIL."
  (awhen (gethash (running-port app) *handlers*)
    ;; The handler exists, so the app is up and running. Stop it, and return t.
    (clack:stop it)
    (setf (running-port app) nil)
    t))
