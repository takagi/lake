#|
  This file is a part of clake project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage clake-test-asd
  (:use :cl :asdf))
(in-package :clake-test-asd)

(defsystem clake-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/clake"
  :depends-on (:clake
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clake"))))
  :description "Test system for clake."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
