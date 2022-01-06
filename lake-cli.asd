#|
  This file is a part of lake project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(defsystem "lake-cli"
  :version "0.1.3"
  :author "Rudolph Miller and Masayuki Takagi"
  :license "MIT"
  :homepage "https://github.com/takagi/lake"
  :depends-on ("lake")
  :description "Lake is a GNU make like build utility in Common Lisp."
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "lake"
  :entry-point "lake/main:uiop-main")
