#|
  This file is a part of lake project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage lake-asd
  (:use :cl :asdf))
(in-package :lake-asd)

(defsystem lake
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/takagi/lake"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :closer-mop
               :alexandria
               :split-sequence
               :cl-syntax-interpol
               #+thread-support
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "lake")
                 #+thread-support
                 (:file "lake-parallel"))))
  :description "Lake is a GNU make like build utility in Common Lisp."
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
  :in-order-to ((test-op (load-op lake-test))))
