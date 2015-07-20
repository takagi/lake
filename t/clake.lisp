(in-package :cl-user)
(defpackage clake-test
  (:use :cl
        :clake
        :prove))
(in-package :clake-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clake)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
