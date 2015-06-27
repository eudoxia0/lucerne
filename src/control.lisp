(in-package :cl-user)
(defpackage lucerne.ctl
  (:use :cl)
  (:import-from :lucerne.app
                :base-app
                :handler
                :build-app)
  (:export :start
           :stop)
  (:documentation "Lucerne keeps a database of running applications and their
port numbers. This is useful when loading a system that starts a Lucerne
application, but either does not shut it down or fails to do so because of an
error -- This is particularly common in testing. By using this system, we don't
leak ports and prevent 'address in use' errors."))
(in-package :lucerne.ctl)

(defmethod start ((app base-app) &key (port 8000) (server :hunchentoot) debug)
  "Bring up @cl:param(app), by default on @cl:param(port) 8000. If the server
was not running, it returns @c(T). If the server was running, it restarts it and
returns @c(NIL)."
  (let ((rebooted nil))
    (when (handler app)
      ;; The handler already exists, meaning the server is running. Bring it
      ;; down before bringing it up again.
      (setf rebooted t)
      (clack:stop (handler app)))
    (setf (handler app)
          (clack:clackup
           (lack:builder (let ((clack-app (build-app app)))
                           (if debug
                               (funcall clack-errors:*clack-error-middleware*
                                        clack-app
                                        :debug t)
                               clack-app)))
           :port port
           :server server
           :use-default-middlewares nil))
    (sleep 1)
    ;; If it was rebooted, return nil. Otherwise t.
    (not rebooted)))

(defmethod stop ((app base-app))
  "If @cl:param(app) is running, stop it and return @c(T). Otherwise, do nothing
and return @c(NIL)."
  (if (handler app)
      ;; The handler exists, so the app is up and running. Stop it, and return t.
      (progn
        (clack:stop (handler app))
        (sleep 1)
        (setf (handler app) nil)
        t)
      ;; Not running
      nil))
