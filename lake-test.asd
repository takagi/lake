#|
  This file is a part of lake project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage lake-test-asd
  (:use :cl :asdf))
(in-package :lake-test-asd)

(defsystem lake-test
  :author "Rudolph Miller and Masayuki Takagi"
  :license "MIT"
  :homepage "https://github.com/takagi/lake"
  :depends-on (:lake
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "lake"))))
  :description "Test system for lake."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
