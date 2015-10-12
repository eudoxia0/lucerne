(in-package :lucerne-test)

(defun run-tests ()
  (run! 'basic)
  (run! 'subapps)
  (run! 'lucerne-test.examples:examples)
  (run! 'lucerne-test.skeleton:skeleton))
