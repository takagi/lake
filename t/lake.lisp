(in-package :cl-user)
(defpackage lake-test
  (:use :cl
        :lake
        :prove)
  (:shadowing-import-from :lake
                          :directory)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :uiop
                :getcwd
                :chdir
                :directory-exists-p))
(in-package :lake-test)

(plan nil)

(defun noop ())

(defmacro with-test-directory (&body body)
  (with-gensyms (olddir pathname)
    `(let ((,olddir (getcwd))
           (,pathname (merge-pathnames "t/"
                       (asdf:system-source-directory :lake-test))))
       (let ((*default-pathname-defaults* ,pathname))
         (chdir ,pathname)
         (unwind-protect
              ,@body
           (sh "rm -f foo bar")
           (sh "rm -f hello.c hello.o")
           (sh "rm -rf dir")
           (chdir ,olddir))))))

#+ros.init
(defmacro with-environment-variable ((var value) &body body)
  (with-gensyms (orig-env)
    `(let ((,orig-env (ros:getenv ,var)))
       (ros:setenv ,var ,value)
       (unwind-protect (progn ,@body)
         (if ,orig-env
             (ros:setenv ,var ,orig-env)
             (ros:unsetenv ,var))))))

#+ros.init
(defmacro with-environment-variables (pairs &body body)
  (if pairs
      (destructuring-bind (pair . rest) pairs
        `(with-environment-variable ,pair
           (with-environment-variables ,rest
             ,@body)))
      `(progn ,@body)))


;;
;; Utilities

(subtest "ensure-pair"

  (is (lake::ensure-pair 'foo)
      '(foo nil))

  (is (lake::ensure-pair '(1 2))
      '(1 2))

  (is-error (lake::ensure-pair nil)
            simple-error
            "invalid value.")

  (is-error (lake::ensure-pair '(foo))
            simple-error
            "invalid value.")

  (is-error (lake::ensure-pair '(foo 1 2))
            simple-error
            "invalid value."))

(subtest "valid-name-part-p"

  (is (lake::valid-name-part-p "foo")
      t)

  (is (lake::valid-name-part-p "")
      nil)

  (is (lake::valid-name-part-p "foo:bar")
      nil)

  (is-error (lake::valid-name-part-p :foo)
            type-error
            "invalid name part."))


;;
;; Verbose

(subtest "verbose"

  (let ((lake::*verbose* nil))
    (is-print (lake::verbose "foo" nil *standard-output*)
              ""
              "base case 1.")
    (is-print (lake::verbose "foo" t *standard-output*)
              ""
              "base case 2."))

  (let ((lake::*verbose* t))
    (is-print (lake::verbose "foo" nil *standard-output*)
              "foo"
              "base case 3.")
    (is-print (lake::verbose "foo" t *standard-output*)
              (format nil "foo~%")
              "base case 4."))

  (is-error (lake::verbose :foo nil *standard-output*)
            type-error
            "invlalid string.")

  (is-error (lake::verbose :foo t *standard-output*)
            type-error
            "invlalid string.")

  (is-error (lake::verbose "foo" nil :foo)
            type-error
            "invalid stream."))


;;
;; FQTN

(subtest "valid-fqtn-p"

  (is (lake::valid-fqtn-p "foo:bar:baz")
      t)

  (is (lake::valid-fqtn-p "")
      nil)

  (is (lake::valid-fqtn-p "foo:bar:")
      nil)

  (is (lake::valid-fqtn-p ":foo:bar:baz")
      nil)

  (is-error (lake::valid-fqtn-p :foo)
            type-error
            "invalid type."))

(subtest "fqtn-namespace"

  (is (lake::fqtn-namespace "foo:bar:baz")
      '("bar" "foo"))

  (is (lake::fqtn-namespace "foo")
      nil)

  (is-error (lake::fqtn-namespace "foo::baz")
            simple-error
            "invalid value.")

  (is-error (lake::fqtn-namespace :foo)
            type-error
            "invalid type."))

(subtest "fqtn-endname"

  (is (lake::fqtn-endname "foo:bar:baz")
      "baz")

  (is (lake::fqtn-endname "foo")
      "foo")

  (is-error (lake::fqtn-endname "foo::baz")
            simple-error
            "invalid value.")

  (is-error (lake::fqtn-endname :foo)
            type-error
            "invalid type."))


;;
;; Namespace

(subtest "valid-task-name-p"

  (is (lake::valid-task-name-p "foo")
      t)

  (is (lake::valid-task-name-p "")
      nil)

  (is (lake::valid-task-name-p "foo:bar")
      nil)

  (is-error (lake::valid-task-name-p :foo)
            type-error
            "invalid task name."))

(subtest "valid-namespace-p"

  (is (lake::valid-namespace-p '("bar" "foo"))
      t)

  (is (lake::valid-namespace-p '("foo:bar"))
      nil)

  (is-error (lake::valid-namespace-p :foo)
            type-error
            "invalid namespace."))

(subtest "valid-dependency-task-name-p"

  (is (lake::valid-dependency-task-name-p "foo")
      t)

  (is (lake::valid-dependency-task-name-p "foo:bar")
      t)

  (is (lake::valid-dependency-task-name-p ":foo:bar")
      t)

  (is (lake::valid-dependency-task-name-p "")
      nil)

  (is (lake::valid-dependency-task-name-p ":")
      nil)

  (is (lake::valid-dependency-task-name-p "foo:")
      nil)

  (is-error (lake::valid-dependency-task-name-p :foo)
            type-error
            "invalid task-name."))

(subtest "resolve-task-name"

  (is (lake::resolve-task-name "foo" nil)
      "foo")

  (is (lake::resolve-task-name "bar" '("foo"))
      "foo:bar")

  (is (lake::resolve-task-name "baz" '("bar" "foo"))
      "foo:bar:baz")

  (is-error (lake::resolve-task-name :foo nil)
            type-error
            "invalid task name.")

  (is-error (lake::resolve-task-name "foo:bar" nil)
            simple-error
            "invalid task name.")

  (is-error (lake::resolve-task-name "foo" :foo)
            type-error
            "invalid namespace.")

  (is-error (lake::resolve-task-name "foo" '("foo:bar"))
            simple-error
            "invalid namespace."))

(subtest "resolve-dependency-task-name"

  (is (lake::resolve-dependency-task-name "foo" nil)
      "foo")

  (is (lake::resolve-dependency-task-name "foo:bar" nil)
      "foo:bar")

  (is (lake::resolve-dependency-task-name "bar" '("foo"))
      "foo:bar")

  (is (lake::resolve-dependency-task-name "bar:baz" '("foo"))
      "foo:bar:baz")

  (is (lake::resolve-dependency-task-name ":foo:bar" '("foo"))
      "foo:bar")

  (is (lake::resolve-dependency-task-name "baz" '("bar" "foo"))
      "foo:bar:baz")

  (is-error (lake::resolve-dependency-task-name :foo nil)
            type-error
            "invalid dependency task name.")

  (is-error (lake::resolve-dependency-task-name "foo:bar:" nil)
            simple-error
            "invalid dependency task name.")

  (is-error (lake::resolve-dependency-task-name "foo" :foo)
            type-error
            "invalid namespace.")

  (is-error (lake::resolve-dependency-task-name "foo" '("foo:bar"))
            simple-error
            "invalid namespace."))


;;
;; Task

(subtest "arg-pair-p"

  (is (lake::arg-pair-p '(foo 1))
      t)

  (is (lake::arg-pair-p :foo)
      nil)

  (is (lake::arg-pair-p nil)
      nil)

  (is (lake::arg-pair-p '(foo))
      nil)

  (is (lake::arg-pair-p '(1 1))
      nil)

  (is (lake::arg-pair-p '(foo 1 2))
      nil))

(subtest "task"

  (let ((task (lake::make-task "bar" '("foo") nil '("baz" ":baz") "desc"
                               #'(lambda () (echo "foo")))))
    (is (lake::task-name task)
        "foo:bar")
    (is (lake::task-dependency task)
        '("foo:baz" "baz"))
    (is (lake::task-description task)
        "desc")
    (is-print (funcall (lake::task-action task))
              (format nil "foo~%")
              "base case 1.")
    (is-print (print-object task *standard-output*)
              "#<TASK foo:bar>"
              "base case 2."))

  (is-error (lake::make-task :foo nil nil nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (lake::make-task "foo:bar" nil nil nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (lake::make-task "foo" :foo nil nil nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" '("foo:bar") nil nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" '(":foo") nil nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" nil :foo nil nil #'noop)
            type-error
            "invalid arguments.")

  (is-error (lake::make-task "foo" nil nil :foo nil #'noop)
            type-error
            "invalid dependency.")

  (is-error (lake::make-task "foo" nil nil nil :foo #'noop)
            type-error
            "invalid description.")

  (is-error (lake::make-task "foo" nil nil nil nil :foo)
            type-error
            "invalid task action."))

(subtest "execute-task - task"

  (let ((task (lake::make-task "foo" nil nil nil nil
                               #'(lambda () (echo "foo")))))
    (is-print (lake::execute-task task)
              (format nil "foo~%")
              "base case 1."))

  (let ((task (lake::make-task "foo" nil '(foo) nil nil
                               #'(lambda (foo) (echo foo)))))
    (is-print (lake::execute-task task '("123"))
              (format nil "123~%")
              "base case 2."))

  (is-error (lake::execute-task :foo)
            simple-error
            "invalid task."))

;;
;; File task

(subtest "file-task"

  (let ((task (lake::make-file-task "hello.o" '("hello") nil '("hello.c") "desc"
                                    #'(lambda ()
                                        (sh "gcc -c hello.c")))))
    (is (lake::task-name task)
        "hello:hello.o")
    (is (lake::task-dependency task)
        '("hello:hello.c"))
    (is (lake::task-description task)
        "desc"))

  (is-error (lake::make-file-task :foo nil nil nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (lake::make-file-task "hello:hello.o" nil nil nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (lake::make-file-task "hello.o" :foo nil nil nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" '("foo:bar") nil nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" '(":foo") nil nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" nil :foo nil nil #'noop)
            type-error
            "invalid arguments.")

  (is-error (lake::make-file-task "hello.o" nil nil :foo nil #'noop)
            type-error
            "invalid dependency.")

  (is-error (lake::make-file-task "hello.o" nil nil nil :foo #'noop)
            type-error
            "invalid task description.")

  (is-error (lake::make-file-task "hello.o" nil nil nil nil :foo)
            type-error
            "invalid task action."))

(subtest "file-task-out-of-date"

  (with-test-directory
    (let ((task (lake::make-file-task "foo" nil nil '("bar") nil #'noop)))
      (sh "touch foo; sleep 1; touch bar")
      (is (lake::file-task-out-of-date task)
          t)
      (sh "touch foo")
      (is (lake::file-task-out-of-date task)
          nil)))

  (let ((task (lake::make-file-task "foo" nil nil '("bar") nil #'noop)))
    (is-error (lake::file-task-out-of-date task)
              error
              "no target file exists."))

  (with-test-directory
    (let ((task (lake::make-file-task "foo" nil nil '("bar") nil #'noop)))
      (sh "touch foo")
      (is-error (lake::file-task-out-of-date task)
                error
                "no dependency file exists.")))

  (is-error (lake::file-task-out-of-date :foo)
            simple-error
            "invalid file task."))

(subtest "execute-task - file-task"

  (with-test-directory
    (let ((task1 (lake::make-file-task "foo" nil nil '("bar") nil
                                       #'(lambda ()
                                           (sh "touch foo")
                                           (echo "foo")))))
      (sh "touch bar")
      (is-print (lake::execute-task task1)
                (format nil "foo~%")
                "base case 1.")
      (is-print (lake::execute-task task1)
                ""
                "base case 2.")))

  (with-test-directory
    (let ((task (lake::make-file-task "foo" nil '(foo) nil nil
                                      #'(lambda (foo)
                                          (echo foo)))))
      (is-print (lake::execute-task task '("123"))
                (format nil "123~%")
                "base case 3."))))


;;
;; Directory task

(subtest "directory-task"

  (let ((task (lake::make-directory-task "dir" '("foo") "desc")))
    (is (lake::task-name task)
        "foo:dir")
    (is (lake::task-description task)
        "desc"))

  (is-error (lake::make-directory-task :foo nil nil)
            type-error
            "invalid directory task name.")

  (is-error (lake::make-directory-task "foo:dir" nil nil)
            simple-error
            "invalid directory task name.")

  (is-error (lake::make-directory-task "dir" :foo nil)
            type-error
            "invalid namespace.")

  (is-error (lake::make-directory-task "dir" '("foo:") nil)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-directory-task "dir" '("foo:") :foo)
            type-error
            "invalid description."))

(subtest "execute-task - directory task"

  (with-test-directory
    (let ((task (lake::make-directory-task "dir" nil nil)))
      (lake::execute-task task)
      (is (and (directory-exists-p "dir") t)
          t)))

  (with-test-directory
    (let ((task (lake::make-directory-task "dir" nil nil)))
      (sh "mkdir dir")
      (lake::execute-task task)
      (is (and (directory-exists-p "dir") t)
          t))))

(subtest "directory-task-directory-name"

  (let ((task (lake::make-directory-task "dir" '("foo") "desc")))
    (is (lake::directory-task-directory-name task)
        "dir"))

  (is-error (lake::directory-task-directory-name :foo)
            type-error
            "invalid directory task."))


;;
;; Macros

(subtest "namespace macro"

  (let ((lake::*tasks* nil))

    (namespace "foo"
      (namespace "bar"
        (task "baz" ()
          (echo "foo.bar.baz"))))

    (is-print (lake::run-task "foo:bar:baz" lake::*tasks*)
              (format nil "foo.bar.baz~%")
              "basic case 1.")

    (is-error (macroexpand '(namespace (format nil "foo")
                              (task "bar" ()
                                (echo "foo.bar"))))
              type-error
              "invalid namespace.")))

(subtest "parse-body"

  (is-values (lake::parse-body '((echo "foo") (echo "bar")))
             '(((echo "foo") (echo "bar")) nil)
             "base case 1.")

  (is-values (lake::parse-body '("description" (echo "foo")))
             '(((echo "foo")) "description")
             "base case 2.")

  (is-values (lake::parse-body nil)
             '(nil nil)
             "base case 3."))

(subtest "task macro"

  (let ((lake::*tasks* nil))

    (task "foo" ()
      "desc"
      (echo "foo"))

    (is-print (lake::run-task "foo" lake::*tasks*)
              (format nil "foo~%")
              "base case 1.")

    (is-error (macroexpand '(task (format nil "foo") ()
                             (echo "foo")))
              type-error
              "invalid task name.")))

(subtest "file macro"

  (let ((lake::*tasks* nil))

    (file "hello.o" ("hello.c")
      "desc"
      (echo "gcc -c hello.c"))

    (with-test-directory
      (sh "touch hello.c")
      (is-print (lake::run-task "hello.o" lake::*tasks*)
                (format nil "gcc -c hello.c~%")
                "base case 1."))

    (is-error (macroexpand '(file (format nil "hello.o") ("hello.c")
                             (echo "gcc -c hello.c")))
              type-error
              "invalid task name.")))
    
(subtest "directory macro"

  (let ((lake::*tasks* nil))

    (directory "dir" "desc")

    (with-test-directory
      (lake::run-task "dir" lake::*tasks*)
      (is (and (directory-exists-p "dir") t)
          t))

    (is-error (macroexpand '(directory (format nil "dir")))
              type-error
              "invalid task name.")))


;;
;; Echo


;;
;; SH

(subtest "sh"

  (is-print (sh "echo foo" :echo t)
            (format nil "echo foo~%foo~%")
            "base case 1.")

  (is-print (sh '("echo" "foo") :echo t)
            (format nil "echo foo~%foo~%")
            "base case 2.")

  (is-error (sh "[ 1 = 2 ]")
            simple-error
            "Exit with error code."))


;;
;; SSH

(subtest "escape-for-shell"

  (is (lake::escape-for-shell "\$PATH")
      "\\$PATH")

  (is (lake::escape-for-shell "\"foo\"")
      "\\\"foo\\\"")

  (is (lake::escape-for-shell "\\")
      "\\\\")

  (is-error (lake::escape-for-shell :foo)
            type-error
            "Invalid string."))

#+ci
(subtest "ssh"

  (let ((*ssh-host* "localhost")
        (*ssh-user* "`whoami`")
        (*ssh-identity* nil))

    (is-print (ssh "echo foo" :echo t)
              (format nil "ssh -q -t -o \"StrictHostKeyChecking no\" `whoami`@localhost \"/bin/bash -l -c \\\"echo foo\\\"\"~%foo~C~%" #\Return))

    (is-print (ssh '("echo" "foo") :echo t)
              (format nil "ssh -q -t -o \"StrictHostKeyChecking no\" `whoami`@localhost \"/bin/bash -l -c \\\"echo foo\\\"\"~%foo~C~%" #\Return)))

  (is-error (ssh '("echo" "foo"))
            simple-error
            "*SSH-HOST* not specified."))


;;
;; SCP

#+ci
(subtest "scp"

  (let ((*ssh-host* "localhost")
        (*ssh-user* "`whoami`")
        (*ssh-identity* nil))
    (with-test-directory
      (sh "touch foo")
      (scp :local #P"foo" :remote #P"lake/t/bar") ; Assuming on CircleCI.
      (is-print (sh "ls foo bar")
                (format nil "bar~%foo~%"))))

  (let ((*ssh-host* "localhost"))
    (is-error (scp :foo #P"foo" :remote #P"bar")
              simple-error
              "invalid scp place."))

  (let ((*ssh-host* "localhost"))
    (is-error (scp :local :foo :remote #P"bar")
              type-error
              "invalid pathspec."))

  (let ((*ssh-host* "localhost"))
    (is-error (scp :local #P"foo" :foo #P"bar")
              simple-error
              "invalid scp place."))

  (let ((*ssh-host* "localhost"))
    (is-error (scp :local #P"foo" :remote :foo)
              type-error
              "invalid pathspec."))

  (is-error (scp :local #P"foo" :remote :foo)
            simple-error
            "*SSH-HOST* not specified."))


;;
;; GETENV


;;
;; *PATH*


;;
;; Task manager

(subtest "register-task"
  )

(subtest "task-exists-p"

  (let ((tasks nil))

    (lake::register-task (lake::make-task "foo" nil nil nil nil #'noop) tasks)

    (is (lake::task-exists-p "foo" tasks)
        t)

    (is (lake::task-exists-p "bar" tasks)
        nil))

  (is-error (lake::task-exists-p :foo nil)
            type-error
            "invalid task name.")

  (is-error (lake::task-exists-p "foo" :foo)
            type-error
            "invalid task manager."))

(subtest "get-task"

  (let ((tasks nil)
        (task (lake::make-task "foo" nil nil nil nil #'noop)))
    (lake::register-task task tasks)
    (is (lake::get-task "foo" tasks)
        task
        "base case 1."))

  (let ((tasks nil)
        (task1 (lake::make-task "foo" nil nil nil nil #'noop))
        (task2 (lake::make-task "foo" nil nil nil nil #'(lambda ()
                                                          (echo "foo")))))
    (lake::register-task task1 tasks)
    (lake::register-task task2 tasks)
    (is-print (lake::run-task "foo" tasks)
              (format nil "foo~%")
              "base case 2."))

  (is-error (lake::get-task :foo nil)
            type-error
            "invalid task name.")

  (is-error (lake::get-task "foo" nil)
            simple-error
            "no task found.")

  (is-error (lake::get-task "foo" :foo)
            type-error
            "invalid task manager."))

(subtest "get-environment-variable"

  #+ros.init
  (with-environment-variables (("FOO_BAR" "123"))
    (is (lake::get-environment-variable "FOO_BAR")
        "123"))

  #+ros.init
  (with-environment-variables (("foo_bar" "123"))
    (is (lake::get-environment-variable "FOO_BAR")
        "123"))

  #+ros.init
  (with-environment-variables (("Foo_Bar" "123"))
    (is (lake::get-environment-variable "Foo_Bar")
        nil))

  (is (lake::get-environment-variable "Foo_Bar")
      nil)

  (is-error (lake::get-environment-variable :foo)
            type-error
            "invalid name."))

(subtest "get-task-arguments"
  ;; Test GET-TASK-ARGUMENTS based on the following matrix:
  ;; - no arguments / an argument / two or more arguments
  ;; - get value from plist / environment variables / default

  ;; No arguments.
  (let ((task (lake::make-task "foo" nil nil nil nil #'noop)))
    (is (lake::get-task-arguments task nil)
        nil))

  ;; An task argument, get its value from plist.
  (let ((task (lake::make-task "foo" nil '(foo) nil nil #'noop)))
    (is (lake::get-task-arguments task '(foo 123))
        '(123)))

  ;; An task argument, get its value from environment variables.
  #+ros.init
  (with-environment-variables (("FOO" "123"))
    (let ((task (lake::make-task "foo" nil '(foo) nil nil #'noop)))
      (is (lake::get-task-arguments task nil)
          '(123))))

  ;; An task argument, get its value from its default.
  (let ((task (lake::make-task "foo" nil '((foo 123)) nil nil #'noop)))
    (is (lake::get-task-arguments task nil)
        '(123)))

  ;; An task argument, no value for it supplied.
  (let ((task (lake::make-task "foo" nil '(foo) nil nil #'noop)))
    (is (lake::get-task-arguments task nil)
        '(nil)))

  ;; Two or more arguments.
  (let ((task (lake::make-task "foo" nil '(foo bar) nil nil #'noop)))
    (is (lake::get-task-arguments task '(foo 1 bar 2))
        '(1 2)))

  (is-error (lake::get-task-arguments :foo nil)
            type-error
            "invalid task.")

  (let ((task (lake::make-task "foo" nil nil nil nil #'noop)))
    (is-error (lake::get-task-arguments task :foo)
              type-error
              "invalid arguments.")))

(subtest "traverse-tasks"

  (let ((tasks nil)
        (task1 (lake::make-task "a" nil nil '("b" "c") nil #'noop)))
    (lake::register-task task1 tasks)
    (is (lake::traverse-tasks "a" tasks)
        '("b" "c" "a")))

  (let ((tasks nil)
        (task1 (lake::make-task "a" nil nil '("b" "c") nil #'noop))
        (task2 (lake::make-task "b" nil nil '("d" "e") nil #'noop))
        (task3 (lake::make-task "c" nil nil nil nil #'noop))
        (task4 (lake::make-task "d" nil nil nil nil #'noop))
        (task5 (lake::make-task "e" nil nil nil nil #'noop)))
    (lake::register-task task1 tasks)
    (lake::register-task task2 tasks)
    (lake::register-task task3 tasks)
    (lake::register-task task4 tasks)
    (lake::register-task task5 tasks)
    (is (lake::traverse-tasks "a" tasks)
        '("d" "e" "b" "c" "a")))

  (let ((tasks nil)
        (task1 (lake::make-task "a" nil nil '("a") nil #'noop)))
    (lake::register-task task1 tasks)
    (is-error (lake::traverse-tasks "a" tasks)
              simple-error
              "circular dependency."))

  (is-error (lake::traverse-tasks :foo nil)
            type-error
            "invlaid task name.")

  (is-error (lake::traverse-tasks "foo" :foo)
            type-error
            "invalid tasks."))

(subtest "compute-dependency"

  (let ((tasks nil)
        (task1 (lake::make-task "foo" nil nil '("bar") nil #'noop))
        (task2 (lake::make-task "bar" nil nil nil nil #'noop)))
    (lake::register-task task1 tasks)
    (lake::register-task task2 tasks)
    (is (lake::compute-dependency "foo" tasks)
        (list task2 task1)
        "base case 1."))

  (with-test-directory
    (let ((tasks nil)
          (task1 (lake::make-task "foo" nil nil '("bar") nil #'noop)))
      (lake::register-task task1 tasks)
      (sh "touch bar")
      (is (lake::compute-dependency "foo" tasks)
          (list task1)
          "base case 2.")))

  (let ((tasks nil)
        (task1 (lake::make-task "foo" nil nil '("bar") nil #'noop)))
    (lake::register-task task1 tasks)
    (is-error (lake::compute-dependency "foo" tasks)
              simple-error
              "unknown task."))

  (let ((tasks nil)
        (task1 (lake::make-task "foo" nil nil '("foo") nil #'noop)))
    (lake::register-task task1 tasks)
    (is-error (lake::compute-dependency "foo" tasks)
              simple-error
              "circular dependency."))

  (let ((tasks nil)
        (task1 (lake::make-task "multi" nil nil '("a" "b" "c") nil #'noop))
        (task2 (lake::make-task "a" nil nil '("b") nil #'noop))
        (task3 (lake::make-task "b" nil nil '("c") nil #'noop))
        (task4 (lake::make-task "c" nil nil nil nil #'noop)))
    (lake::register-task task1 tasks)
    (lake::register-task task2 tasks)
    (lake::register-task task3 tasks)
    (lake::register-task task4 tasks)
    (is (lake::compute-dependency "multi" tasks)
        (list task4 task3 task2 task1)
        "base case 3."))

  (is-error (lake::compute-dependency :foo nil)
            type-error
            "invalid task name.")

  (is-error (lake::compute-dependency "foo" :foo)
            type-error
            "invalid tasks."))

(subtest "parse-target"
  ;; no arguments / an argument / two or more argument

  (is-values (lake::parse-target "foo")
             '("foo" nil))

  (is-values (lake::parse-target "foo[]")
             '("foo" nil))

  (is-values (lake::parse-target "foo[123]")
             '("foo" ("123")))

  (is-values (lake::parse-target "foo[123,456]")
             '("foo" ("123" "456")))

  (is-values (lake::parse-target "foo[ 123, 456 ]")
             '("foo" (" 123" "456")))

  (is-values (lake::parse-target "foo[123,4 56]")
             '("foo" ("123" "4 56")))

  (is-values (lake::parse-target "foo[123]]")
             '("foo" ("123]")))

  (is-values (lake::parse-target "foo[123]1")
             '("foo[123]1" nil))

  (is-values (lake::parse-target "foo[\"123,456\"]")
             '("foo" ("\"123" "456\"")))

  (is-values (lake::parse-target "foo[123\\,4\\,56]")
             '("foo" ("123,4,56")))

  (is-values (lake::parse-target "foo[123")
             '("foo[123" nil))

  (is-error (lake::parse-target :foo)
            type-error
            "invalid type."))

(subtest "run-task"

  (let ((tasks nil)
        (result nil))
    ;; Register task A.
    (let ((task (lake::make-task "a" nil nil '("b" "c") nil #'noop)))
      (lake::register-task task tasks))
    ;; Register task B.
    (let ((task (lake::make-task "b" nil nil nil nil
                                 #'(lambda ()
                                     (sleep 1)
                                     (push :b result)))))
      (lake::register-task task tasks))
    ;; Register task C.
    (let ((task (lake::make-task "c" nil nil nil nil
                                 #'(lambda ()
                                     (push :c result)))))
      (lake::register-task task tasks))
    ;; Test serial run.
    (lake::run-task "a" tasks 1)
    (is result '(:c :b))
    ;; Test concurrent run.
    #+thread-support
    (progn
      (lake::run-task "a" tasks 2)
      (is result '(:b :c :c :b)))))

(subtest "execute"

  (let ((lake::*tasks* nil))

    (namespace "hello1"

      (task "foo" ()
        (echo "foo")
        (execute "bar"))

      (task "bar" ()
        (echo "bar")))

    (namespace "hello2"

      (file "hello" ()
        (echo "hello")
        (execute "world"))

      (file "world" ()
        (echo "world")))

    (namespace "hello3"

      (task "foo" ()
        (echo "foo")
        (execute ":bar")))

    (task "bar" ()
      (echo "bar"))

    (namespace "hello4"

      (task "foo" ()
        (execute "bar"))

      (task "bar" ()
        (ssh "echo bar")))

    (is-print (lake::run-task "hello1:foo" lake::*tasks*)
              (format nil "foo~%bar~%")
              "basic case 1.")

    (is-print (lake::run-task "hello2:hello" lake::*tasks*)
              (format nil "hello~%world~%")
              "basic case 2.")

    (is-print (lake::run-task "hello3:foo" lake::*tasks*)
              (format nil "foo~%bar~%")
              "basic case 3.")

    #+ci
    (let ((*ssh-host* "localhost")
          (*ssh-user* "`whoami`")
          (*ssh-identity* nil))
      (is-print (lake::run-task "hello4:foo" lake::*tasks*)
                (format nil "bar~C~%" #\Return)
                "Ok. - Inheriting SSH related special variables"))

    (is-error (lake::run-task :foo lake::*tasks*)
              type-error
              "invalid task name.")

    (is-error (lake::run-task "foobar" lake::*tasks*)
              simple-error
              "no task.")

    ;; (is-error (execute "bar")
    ;;           simple-error
    ;;           "outside task.")))
    )

  (let ((tasks nil)
        (result nil))
    ;; Register task A.
    (let ((task (lake::make-task "a" nil nil nil nil
                                 #'(lambda ()
                                     (execute "foo:b")))))
      (lake::register-task task tasks))
    ;; Register task B.
    (let ((task (lake::make-task "b" '("foo") nil '("c" "d") nil #'noop)))
      (lake::register-task task tasks))
    ;; Register task C.
    (let ((task (lake::make-task "c" '("foo") nil nil nil
                                 #'(lambda ()
                                     (sleep 1)
                                     (push :c result)))))
      (lake::register-task task tasks))
    ;; Register task D.
    (let ((task (lake::make-task "d" '("foo") nil nil nil
                                 #'(lambda ()
                                     (push :d result)))))
      (lake::register-task task tasks))
    ;; Test serial execution.
    (lake::run-task "a" tasks 1)
    (is result '(:d :c))
    ;; Test concurrent execution.
    #+thread-support
    (progn
      (lake::run-task "a" tasks 2)
      (is result '(:c :d :d :c)))))


;;
;; Lake



(finalize)
