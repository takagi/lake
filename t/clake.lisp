(in-package :cl-user)
(defpackage clake-test
  (:use :cl
        :clake
        :prove)
  (:shadowing-import-from :clake
                          :directory)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :uiop
                :getcwd
                :chdir
                :delete-file-if-exists
                :delete-empty-directory
                :directory-exists-p))
(in-package :clake-test)

(plan nil)

(defun noop ())

(defmacro with-test-directory (&body body)
  (with-gensyms (olddir pathname)
    `(let ((,olddir (getcwd))
           (,pathname (merge-pathnames "t/"
                       (asdf:system-source-directory :clake-test))))
       (let ((*default-pathname-defaults* ,pathname))
         (chdir ,pathname)
         (unwind-protect
              ,@body
           (sh "rm -f foo bar")
           (sh "rm -f hello.c hello.o")
           (sh "rm -rf dir")
           (chdir ,olddir))))))


;;;
;;; Utilities
;;;

(subtest "last1"

  (is (clake::last1 '(1 2 3))
      3)

  (is-error (clake::last1 :foo)
            type-error
            "invalid list."))


;;;
;;; Namespace
;;;

(subtest "valid-task-name-p"

  (is (clake::valid-task-name-p "foo")
      t)

  (is (clake::valid-task-name-p "")
      nil)

  (is (clake::valid-task-name-p "foo:bar")
      nil)

  (is-error (clake::valid-task-name-p :foo)
            type-error
            "invalid task name."))

(subtest "valid-namespace-p"

  (is (clake::valid-namespace-p '("bar" "foo"))
      t)

  (is (clake::valid-namespace-p '("foo:bar"))
      nil)

  (is-error (clake::valid-namespace-p :foo)
            type-error
            "invalid namespace."))

(subtest "resolve-task-name"

  (is (clake::resolve-task-name "foo" nil)
      "foo")

  (is (clake::resolve-task-name "bar" '("foo"))
      "foo:bar")

  (is (clake::resolve-task-name "baz" '("bar" "foo"))
      "foo:bar:baz")

  (is-error (clake::resolve-task-name :foo nil)
            type-error
            "invalid task name.")

  (is-error (clake::resolve-task-name "foo:bar" nil)
            simple-error
            "invalid task name.")

  (is-error (clake::resolve-task-name "foo" :foo)
            type-error
            "invalid namespace.")

  (is-error (clake::resolve-task-name "foo" '("foo:bar"))
            simple-error
            "invalid namespace."))

(subtest "valid-dependency-task-name-p"

  (is (clake::valid-dependency-task-name-p "foo")
      t)

  (is (clake::valid-dependency-task-name-p "foo:bar")
      t)

  (is (clake::valid-dependency-task-name-p ":foo:bar")
      t)

  (is (clake::valid-dependency-task-name-p "")
      nil)

  (is (clake::valid-dependency-task-name-p ":")
      nil)

  (is (clake::valid-dependency-task-name-p "foo:")
      nil)

  (is-error (clake::valid-dependency-task-name-p :foo)
            type-error
            "invalid task-name."))

(subtest "resolve-dependency-task-name"

  (is (clake::resolve-dependency-task-name "foo" nil)
      "foo")

  (is (clake::resolve-dependency-task-name "foo:bar" nil)
      "foo:bar")

  (is (clake::resolve-dependency-task-name "bar" '("foo"))
      "foo:bar")

  (is (clake::resolve-dependency-task-name "bar:baz" '("foo"))
      "foo:bar:baz")

  (is (clake::resolve-dependency-task-name ":foo:bar" '("foo"))
      "foo:bar")

  (is (clake::resolve-dependency-task-name "baz" '("bar" "foo"))
      "foo:bar:baz")

  (is-error (clake::resolve-dependency-task-name :foo nil)
            type-error
            "invalid dependency task name.")

  (is-error (clake::resolve-dependency-task-name "foo:bar:" nil)
            simple-error
            "invalid dependency task name.")

  (is-error (clake::resolve-dependency-task-name "foo" :foo)
            type-error
            "invalid namespace.")

  (is-error (clake::resolve-dependency-task-name "foo" '("foo:bar"))
            simple-error
            "invalid namespace."))

(subtest "namespace macro"
  (let ((clake::*tasks* nil))
    (namespace "foo"
      (namespace "bar"
        (task "baz" ()
          (echo "foo.bar.baz")))
    (is-print (clake::%execute-task (clake::get-task "foo:bar:baz"))
              (format nil "foo.bar.baz~%")))))


;;;
;;; Base task
;;;


;;;
;;; Task
;;;

(subtest "task"

  (let ((task (clake::make-task "bar" '("foo") '("baz" ":baz")
                                #'(lambda () (echo "foo")))))
    (is (clake::task-name task)
        "foo:bar")
    (is (clake::task-dependency task)
        '("foo:baz" "baz"))
    (is-print (funcall (clake::task-action task))
              (format nil "foo~%")))

  (is-error (clake::make-task :foo nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (clake::make-task "foo:bar" nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (clake::make-task "foo" :foo nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (clake::make-task "foo" '("foo:bar") nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (clake::make-task "foo" '(":foo") nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (clake::make-task "foo" nil :foo #'noop)
            type-error
            "invalid dependency.")

  (is-error (clake::make-task "foo" nil nil :foo)
            type-error
            "invalid task action."))

(subtest "execute-task"

  (let ((task (clake::make-task "foo" nil nil #'(lambda ()
                                                  (echo "foo")))))
    (clake::register-task task)
    (is-print (clake::%execute-task task)
              (format nil "foo~%")))

  (let ((clake::*tasks* nil)
        (task1 (clake::make-task "bar" '("foo") '("baz")
                                 #'(lambda ()
                                     (echo "foo:bar"))))
        (task2 (clake::make-task "baz" '("foo") nil
                                 #'(lambda ()
                                     (echo "foo:baz")))))
    (clake::register-task task1)
    (clake::register-task task2)
    (is-print (clake::%execute-task task1)
              (format nil "foo:baz~%foo:bar~%")))

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task (clake::make-task "hello.o" nil '("hello.c")
                                  #'(lambda ()
                                      (echo "hello.o")))))
      (clake::register-task task)
      (sh "touch hello.c")
      (is-print (clake::%execute-task task)
                (format nil "hello.o~%"))))

  (is-error (clake::%execute-task :foo)
            simple-error
            "invalid task.")

  (let ((clake::*tasks* nil)
        (task1 (clake::make-task "foo" nil '("bar") #'noop))
        (task2 (clake::make-task "bar" nil '("foo") #'noop)))
    (clake::register-task task1)
    (clake::register-task task2)
    (is-error (clake::%execute-task task1)
              simple-error
              "circular dependency."))

  (let ((clake::*tasks* nil)
        (task1 (clake::make-task "foo" nil '("bar") #'noop)))
    (clake::register-task task1)
    (is-error (clake::%execute-task task1)
              simple-error
              "dependency task not found.")))

(subtest "task macro"

  (let ((clake::*tasks* nil))
    (task "foo" ()
      (echo "foo"))
    (is-print (clake::%execute-task (clake::get-task "foo"))
              (format nil "foo~%"))))


;;;
;;; File task
;;;

(subtest "file-task"

  (let ((task (clake::make-file-task "hello.o" '("hello") '("hello.c")
                                     #'(lambda ()
                                         (sh "gcc -c hello.c")))))
    (is (clake::task-name task)
        "hello:hello.o")
    (is (clake::task-dependency task)
        '("hello:hello.c")))

  (is-error (clake::make-file-task :foo nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (clake::make-file-task "hello:hello.o" nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (clake::make-file-task "hello.o" :foo nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (clake::make-file-task "hello.o" '("foo:bar") nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (clake::make-file-task "hello.o" '(":foo") nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (clake::make-file-task "hello.o" nil :foo #'noop)
            type-error
            "invalid dependency.")

  (is-error (clake::make-file-task "hello.o" nil nil :foo)
            type-error
            "invalid task action."))

(subtest "file-task-out-of-date"

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task (clake::make-file-task "foo" nil '("bar") #'noop)))
      (sh "touch foo; sleep 1; touch bar")
      (is (clake::file-task-out-of-date task)
          t)
      (sh "touch foo")
      (is (clake::file-task-out-of-date task)
          nil)))

  (let ((clake::*tasks* nil)
        (task (clake::make-file-task "foo" nil '("bar") #'noop)))
    (is-error (clake::file-task-out-of-date task)
              error
              "no target file exists."))

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task (clake::make-file-task "foo" nil '("bar") #'noop)))
      (sh "touch foo")
      (is-error (clake::file-task-out-of-date task)
                error
                "no dependency file exists.")))

  (is-error (clake::file-task-out-of-date :foo)
            simple-error
            "invalid file task."))

(subtest "execute-task"

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task1 (clake::make-file-task "foo" nil '("bar")
                                        #'(lambda ()
                                            (sh "touch foo")
                                            (echo "foo")))))
      (sh "touch bar")
      (is-print (clake::%execute-task task1)
                (format nil "foo~%"))
      (is-print (clake::%execute-task task1)
                ""))))

(subtest "file-task"

  (with-test-directory
    (let ((clake::*tasks* nil))
      (file "hello.o" ("hello.c")
        (echo "gcc -c hello.c"))
      (sh "touch hello.c")
      (is-print (clake::%execute-task (clake::get-task "hello.o"))
                (format nil "gcc -c hello.c~%")))))


;;;
;;; Directory task
;;;

(subtest "directory-task"

  (let ((task (clake::make-directory-task "dir" '("foo"))))
    (is (clake::task-name task)
        "foo:dir"))

  (is-error (clake::make-directory-task :foo nil)
            type-error
            "invalid directory task name.")

  (is-error (clake::make-directory-task "foo:dir" nil)
            simple-error
            "invalid directory task name.")

  (is-error (clake::make-directory-task "dir" :foo)
            type-error
            "invalid namespace.")

  (is-error (clake::make-directory-task "dir" '("foo:"))
            simple-error
            "invalid namespace."))

(subtest "execute-task"

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task (clake::make-directory-task "dir" nil)))
      (clake::register-task task)
      (clake::%execute-task task)
      (is (and (directory-exists-p "dir") t)
          t)))

  (with-test-directory
    (let ((clake::*tasks* nil)
          (task (clake::make-directory-task "dir" nil)))
      (clake::register-task task)
      (sh "mkdir dir")
      (clake::%execute-task task)
      (is (and (directory-exists-p "dir") t)
          t))))

(subtest "directory-task"

  (with-test-directory
    (let ((clake::*tasks* nil))
      (directory "dir")
      (clake::%execute-task (clake::get-task "dir"))
      (is (and (directory-exists-p "dir") t)
          t))))


;;;
;;; Run
;;;

(subtest "sh"
  (is-print (sh "echo foo" :echo t)
            (format nil "echo foo~%foo~%")))


;;;
;;; Task manager
;;;

(subtest "task-exists-p"

  (let ((tasks nil))

    (clake::register-task (clake::make-task "foo" nil nil #'noop) tasks)

    (is (clake::task-exists-p "foo" tasks)
        t)

    (is (clake::task-exists-p "bar" tasks)
        nil))

  (is-error (clake::task-exists-p :foo)
            type-error
            "invalid task name.")

  (is-error (clake::task-exists-p "foo" :foo)
            type-error
            "invalid task manager."))

(subtest "get-task"

  (let ((tasks nil)
        (task (clake::make-task "foo" nil nil #'noop)))
    (clake::register-task task tasks)
    (is (clake::get-task "foo" tasks)
        task))

  (let ((tasks nil)
        (task1 (clake::make-task "foo" nil nil #'noop))
        (task2 (clake::make-task "foo" nil nil #'(lambda ()
                                                   (echo "foo")))))
    (clake::register-task task1 tasks)
    (clake::register-task task2 tasks)
    (is-print (clake::%execute-task (clake::get-task "foo" tasks))
              (format nil "foo~%")))

  (is-error (clake::get-task :foo nil)
            type-error
            "invalid task name.")

  (is-error (clake::get-task "foo" nil)
            simple-error
            "no task found.")

  (is-error (clake::get-task "foo" :foo)
            type-error
            "invalid task manager."))


;;;
;;; clake
;;;


(finalize)
