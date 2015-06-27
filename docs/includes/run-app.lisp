CL-USER> (ql:quickload :my-app)
To load "my-app":
  Load 1 ASDF system:
    my-app
; Loading "my-app"
[package my-app]............
(:MY-APP)

CL-USER> (lucerne:start my-app:app :port 8000)
To load "clack-handler-hunchentoot":
  Load 1 ASDF system:
    clack-handler-hunchentoot
; Loading "clack-handler-hunchentoot"

Hunchentoot server is started.
Listening on localhost:8000.
T
CL-USER> 
