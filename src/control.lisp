(in-package :cl-user)
(defpackage lucerne.ctl
  (:use :cl :anaphora)
  (:import-from :lucerne.app
                :<app>
                :handler
                :build-app)
  (:export :start
           :stop))
(in-package :lucerne.ctl)

(defmethod start ((app <app>) &key (port 8000))
  "Bring up `app`, by default on `port` 8000. If the server was not running, it
returns `t`. If the server was running, it restarts it and returns nil."
  (let ((rebooted nil))
    (awhen (handler app)
      ;; The handler already exists, meaning the server is running. Bring it
      ;; down before bringing it up again.
      (setf rebooted t)
      (clack:stop it))
    (let ((handler
            (clack:clackup
             (build-app app)
             :port port
             :server :hunchentoot)))
      (setf (handler app) handler)
      ;; If it was rebooted, return nil. Otherwise t.
      (not rebooted))))

(defmethod stop ((app <app>))
  "If `app` is running, stop it and return T. Otherwise, do nothing and
return NIL."
  (awhen (handler app)
    ;; The handler exists, so the app is up and running. Stop it, and return t.
    (clack:stop it)
    (setf (handler app) nil)
    t))
