(in-package :cl-user)
(defpackage lake
  (:use :cl)
  (:export :lake
           :display-tasks
           :namespace
           :task
           :file
           :directory
           :sh
           :echo
           :execute)
  (:shadow :directory)
  (:import-from :alexandria
                :once-only)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :uiop
                :getcwd
                :run-program
                :file-exists-p))
(in-package :lake)


