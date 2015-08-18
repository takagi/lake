#|
  This file is a part of clake project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

#|
  Clake is a Make-like program implemented in Common Lisp.
  Author: Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage clake-asd
  (:use :cl :asdf))
(in-package :clake-asd)

(defsystem clake
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/clake"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :cl-annot-prove
               :closer-mop
               :alexandria
               :uiop)
  :components ((:module "src"
                :components
                ((:file "clake")
                 (:file "core"))))
  :description "Clake is a Rake-like program implemented in Common Lisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :perform (test-op (op c)
                    (uiop:symbol-call :cl-annot-prove :run-system-tests c)))
