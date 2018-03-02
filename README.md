# Lucerne: a web framework

[![Build Status](https://travis-ci.org/eudoxia0/lucerne.svg?branch=master)](https://travis-ci.org/eudoxia0/lucerne)
[![Coverage Status](https://coveralls.io/repos/eudoxia0/lucerne/badge.svg?branch=master)](https://coveralls.io/r/eudoxia0/lucerne?branch=master)
[![Quicklisp](http://quickdocs.org/badge/lucerne.svg)](http://quickdocs.org/lucerne/)

Read the [docs](http://borretti.me/lucerne/docs/overview.html).

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

Copyright (c) 2014-2018 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
