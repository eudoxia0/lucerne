# Lucerne: A microframework for Common Lisp

[![Build Status](https://travis-ci.org/eudoxia0/lucerne.svg?branch=master)](https://travis-ci.org/eudoxia0/lucerne)

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
