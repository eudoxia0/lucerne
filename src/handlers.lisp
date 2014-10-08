(in-package :lucerne)
(annot:enable-annot-syntax)

(defparameter *handlers*
  (make-hash-table)
  "Maps application names to their handlers.")

(defun start (app &key (port 8000))
  "Bring up `app`, by default on `port` 8000. If the server was not running, it
returns `t`. If the server was running, it restarts it and returns nil."
  (let ((rebooted nil))
    (awhen (gethash app *handlers*)
      ;; The handler already exists, meaning the server is running. Bring it
      ;; down before bringing it up again.
      (setf rebooted t)
      (clack:stop it))
    (let ((handler
            (clack:clackup
             (lucerne::build-app app)
             :port port
             :server :hunchentoot)))
      (setf (gethash app *handlers*) handler)
      ;; If it was rebooted, return nil. Otherwise t.
      (not rebooted))))

(defun stop (app)
  "If `app` is running, stop it and return T. Otherwise, do nothing and
return NIL."
  (awhen (gethash app *handlers*)
    ;; The handler exists, so the app is up and running. Stop it, and return t.
    (clack:stop it)
    (remhash app *handlers*)
    t))
