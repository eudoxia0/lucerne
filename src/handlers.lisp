(in-package :lucerne)
(annot:enable-annot-syntax)

(defparameter *handlers*
  (make-hash-table)
  "Maps application names to their handlers.")

(defmacro start (app &key middleware (port 8000) debug error-function)
  "Bring up `app`, optionally using `middleware`. By default, `port` is 8000 and
`debug` is nil. If the server was not running, it returns `t`. If the server was
running, it restarts it and returns nil."
  `(let ((rebooted nil))
     (awhen (gethash ,app *handlers*)
       ;; The handler already exists, meaning the server is running. Bring it
       ;; down before bringing it up again.
       (setf rebooted t)
       (clack:stop it))
     (let ((handler
             (clack:clackup
              (clack.builder:builder
               (clack.middleware.session:<clack-middleware-session>
                :state
                (make-instance 'clack.session.state.cookie:<clack-session-state-cookie>))
               ,@(if debug
                     (if error-function
                         `((clack-errors:<clack-error-middleware>
                            :fn ,error-function))
                         `(clack-errors:<clack-error-middleware>)))
               ,@middleware
               ,app)
              :port ,port
              :server :hunchentoot)))
       (setf (gethash ,app *handlers*) handler)
       ;; If it was rebooted, return nil. Otherwise t.
       (not rebooted))))

(defmacro stop (app)
  "If `app` is running, stop it and return T. Otherwise, do nothing and
return NIL."
  `(awhen (gethash ,app *handlers*)
     ;; The handler exists, so the app is up and running. Stop it, and return t.
     (clack:stop it)
     (remhash ,app *handlers*)
     t))
