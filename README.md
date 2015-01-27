# Lucerne: A microframework for Common Lisp

## Usage

```lisp
(defapp app)

@route app "/"
(defview index ()
  (respond "<h1>Welcome to Lucerne</h1>"))

@route app "/greet/:name"
(defview greet (name)
  (respond (format nil "Hello, ~A!" name)))

(start app)
```

# License

Copyright (c) 2014-2015 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
