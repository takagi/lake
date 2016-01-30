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
                :delete-file-if-exists
                :delete-empty-directory
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


;;
;; Utilities

(subtest "last1"

  (is (lake::last1 '(1 2 3))
      3)

  (is-error (lake::last1 :foo)
            type-error
            "invalid list."))


;;
;; Verbose

(subtest "verbose"

  (let ((lake::*verbose* nil))
    (is-print (lake::verbose "foo" nil *standard-output*)
              "")
    (is-print (lake::verbose "foo" t *standard-output*)
              ""))

  (let ((lake::*verbose* t))
    (is-print (lake::verbose "foo" nil *standard-output*)
              "foo")
    (is-print (lake::verbose "foo" t *standard-output*)
              (format nil "foo~%")))

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

(subtest "task-name-namespace"

  (is (lake::task-name-namespace "foo:bar:baz")
      '("bar" "foo"))

  (is-error (lake::task-name-namespace :foo)
            type-error))

(subtest "task-name-name"

  (is (lake::task-name-name "foo:bar:baz")
      "baz")

  (is-error (lake::task-name-name :foo)
            type-error))

(subtest "namespace macro"

  (let ((lake::*tasks* nil))
    (namespace "foo"
      (namespace "bar"
        (task "baz" ()
          (echo "foo.bar.baz")))
    (is-print (lake::%execute-task (lake::get-task "foo:bar:baz"))
              (format nil "foo.bar.baz~%"))))

  (is-error (macroexpand '(namespace (format nil "foo")
                            (task "bar" ()
                              (echo "foo.bar"))))
            type-error
            "invalid namespace."))


;;
;; Base task


;;
;; Task

(subtest "task"

  (let ((task (lake::make-task "bar" '("foo") '("baz" ":baz") "desc"
                               #'(lambda () (echo "foo")))))
    (is (lake::task-name task)
        "foo:bar")
    (is (lake::task-dependency task)
        '("foo:baz" "baz"))
    (is (lake::task-description task)
        "desc")
    (is-print (funcall (lake::task-action task))
              (format nil "foo~%")))

  (is-error (lake::make-task :foo nil nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (lake::make-task "foo:bar" nil nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (lake::make-task "foo" :foo nil nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" '("foo:bar") nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" '(":foo") nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-task "foo" nil :foo nil #'noop)
            type-error
            "invalid dependency.")

  (is-error (lake::make-task "foo" nil nil :foo #'noop)
            type-error
            "invalid description.")

  (is-error (lake::make-task "foo" nil nil nil :foo)
            type-error
            "invalid task action."))

(subtest "execute-task"

  (let ((task (lake::make-task "foo" nil nil nil #'(lambda ()
                                                     (echo "foo")))))
    (lake::register-task task)
    (is-print (lake::%execute-task task)
              (format nil "foo~%")))

  (let ((lake::*tasks* nil)
        (task1 (lake::make-task "bar" '("foo") '("baz") nil
                                 #'(lambda ()
                                     (echo "foo:bar"))))
        (task2 (lake::make-task "baz" '("foo") nil nil
                                #'(lambda ()
                                    (echo "foo:baz")))))
    (lake::register-task task1)
    (lake::register-task task2)
    (is-print (lake::%execute-task task1)
              (format nil "foo:baz~%foo:bar~%")))

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task (lake::make-task "hello.o" nil '("hello.c") nil
                                 #'(lambda ()
                                     (echo "hello.o")))))
      (lake::register-task task)
      (sh "touch hello.c")
      (is-print (lake::%execute-task task)
                (format nil "hello.o~%"))))

  (is-error (lake::%execute-task :foo)
            simple-error
            "invalid task.")

  (let ((lake::*tasks* nil)
        (task1 (lake::make-task "foo" nil '("bar") nil #'noop))
        (task2 (lake::make-task "bar" nil '("foo") nil #'noop)))
    (lake::register-task task1)
    (lake::register-task task2)
    (is-error (lake::%execute-task task1)
              simple-error
              "circular dependency."))

  (let ((lake::*tasks* nil)
        (task1 (lake::make-task "foo" nil '("bar") nil #'noop)))
    (lake::register-task task1)
    (is-error (lake::%execute-task task1)
              simple-error
              "dependency task not found.")))

(subtest "parse-body"

  (is-values (lake::parse-body '((echo "foo") (echo "bar")))
             '(((echo "foo") (echo "bar")) nil))

  (is-values (lake::parse-body '("description" (echo "foo")))
             '(((echo "foo")) "description"))

  (is-values (lake::parse-body nil)
             '(nil nil)))

(subtest "task macro"

  (let ((lake::*tasks* nil))
    (task "foo" ()
      "desc"
      (echo "foo"))
    (is-print (lake::%execute-task (lake::get-task "foo"))
              (format nil "foo~%")))

  (is-error (macroexpand '(task (format nil "foo") ()
                            (echo "foo")))
            type-error
            "invalid task name."))


;;
;; File task

(subtest "file-task"

  (let ((task (lake::make-file-task "hello.o" '("hello") '("hello.c") "desc"
                                    #'(lambda ()
                                        (sh "gcc -c hello.c")))))
    (is (lake::task-name task)
        "hello:hello.o")
    (is (lake::task-dependency task)
        '("hello:hello.c"))
    (is (lake::task-description task)
        "desc"))

  (is-error (lake::make-file-task :foo nil nil nil #'noop)
            type-error
            "invalid task name.")

  (is-error (lake::make-file-task "hello:hello.o" nil nil nil #'noop)
            simple-error
            "invalid task name.")

  (is-error (lake::make-file-task "hello.o" :foo nil nil #'noop)
            type-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" '("foo:bar") nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" '(":foo") nil nil #'noop)
            simple-error
            "invalid namespace.")

  (is-error (lake::make-file-task "hello.o" nil :foo nil #'noop)
            type-error
            "invalid dependency.")

  (is-error (lake::make-file-task "hello.o" nil nil :foo #'noop)
            type-error
            "invalid task description.")

  (is-error (lake::make-file-task "hello.o" nil nil nil :foo)
            type-error
            "invalid task action."))

(subtest "file-task-out-of-date"

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task (lake::make-file-task "foo" nil '("bar") nil #'noop)))
      (sh "touch foo; sleep 1; touch bar")
      (is (lake::file-task-out-of-date task)
          t)
      (sh "touch foo")
      (is (lake::file-task-out-of-date task)
          nil)))

  (let ((lake::*tasks* nil)
        (task (lake::make-file-task "foo" nil '("bar") nil #'noop)))
    (is-error (lake::file-task-out-of-date task)
              error
              "no target file exists."))

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task (lake::make-file-task "foo" nil '("bar") nil #'noop)))
      (sh "touch foo")
      (is-error (lake::file-task-out-of-date task)
                error
                "no dependency file exists.")))

  (is-error (lake::file-task-out-of-date :foo)
            simple-error
            "invalid file task."))

(subtest "execute-task"

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task1 (lake::make-file-task "foo" nil '("bar") nil
                                       #'(lambda ()
                                           (sh "touch foo")
                                           (echo "foo")))))
      (sh "touch bar")
      (is-print (lake::%execute-task task1)
                (format nil "foo~%"))
      (is-print (lake::%execute-task task1)
                ""))))

(subtest "file macro"

  (with-test-directory
    (let ((lake::*tasks* nil))
      (file "hello.o" ("hello.c")
        "desc"
        (echo "gcc -c hello.c"))
      (sh "touch hello.c")
      (is-print (lake::%execute-task (lake::get-task "hello.o"))
                (format nil "gcc -c hello.c~%"))))

  (is-error (macroexpand '(file (format nil "hello.o") ("hello.c")
                            (echo "gcc -c hello.c")))
            type-error
            "invalid task name."))


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

(subtest "execute-task"

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task (lake::make-directory-task "dir" nil nil)))
      (lake::register-task task)
      (lake::%execute-task task)
      (is (and (directory-exists-p "dir") t)
          t)))

  (with-test-directory
    (let ((lake::*tasks* nil)
          (task (lake::make-directory-task "dir" nil nil)))
      (lake::register-task task)
      (sh "mkdir dir")
      (lake::%execute-task task)
      (is (and (directory-exists-p "dir") t)
          t))))

(subtest "directory macro"

  (with-test-directory
    (let ((lake::*tasks* nil))
      (directory "dir" "desc")
      (lake::%execute-task (lake::get-task "dir"))
      (is (and (directory-exists-p "dir") t)
          t)))

  (is-error (macroexpand '(directory (format nil "dir")))
            type-error
            "invalid task name."))


;;
;; Echo


;;
;; SH

(subtest "sh"

  (is-print (sh "echo foo" :echo t)
            (format nil "echo foo~%foo~%"))

  (is-print (sh '("echo" "foo") :echo t)
            (format nil "echo foo~%foo~%"))

  (is-error (sh "[ 1 = 2 ]")
            simple-error
            "Exit with error code."))


;;
;; SSH

(subtest "ssh"

  (let ((*ssh-host* "localhost")
        (*ssh-user* "`whoami`")
        (*ssh-identity* nil))

    (is-print (ssh "echo foo" :echo t)
              (format nil "ssh -o \"StrictHostKeyChecking no\" `whoami`@localhost \"echo foo\"~%foo~%"))

    (is-print (ssh '("echo" "foo") :echo t)
              (format nil "ssh -o \"StrictHostKeyChecking no\" `whoami`@localhost \"echo foo\"~%foo~%"))))


;;
;; SCP

(subtest "scp"

  (let ((*ssh-host* "localhost")
        (*ssh-user* "`whoami`")
        (*ssh-identity* nil))
    (with-test-directory
      (sh "touch foo")
      (scp :local #P"foo" :remote #P"lake/t/bar") ; Assuming on CircleCI.
      (is-print (sh "ls foo bar")
                (format nil "bar~%foo~%"))))

  (is-error (scp :foo #P"foo" :remote #P"bar")
            simple-error
            "invalid scp place.")

  (is-error (scp :local :foo :remote #P"bar")
            type-error
            "invalid pathspec.")

  (is-error (scp :local #P"foo" :foo #P"bar")
            simple-error
            "invalid scp place.")

  (is-error (scp :local #P"foo" :remote :foo)
            type-error
            "invalid pathspec."))


;;
;; Execute

(subtest "execute"

  (let ((lake::*tasks* nil))
    (namespace "hello"
      (task "foo" ()
        (echo "foo")
        (execute "bar"))
      (task "bar" ()
        (echo "bar")))
    (is-print (lake::%execute-task (lake::get-task "hello:foo"))
              (format nil "foo~%bar~%")))

  (let ((lake::*tasks* nil))
    (namespace "hello"
      (file "hello" ()
        (echo "hello")
        (execute "world"))
      (file "world" ()
        (echo "world")))
    (is-print (lake::%execute-task (lake::get-task "hello:hello"))
              (format nil "hello~%world~%")))

  (let ((lake::*tasks* nil))
    (namespace "hello"
      (task "foo" ()
        (echo "foo")
        (execute ":bar")))
    (task "bar" ()
      (echo "bar"))
    (is-print (lake::%execute-task (lake::get-task "hello:foo"))
              (format nil "foo~%bar~%")))

  (let ((lake::*namespace* nil))
    (is-error (execute :foo)
              type-error
              "invalid task name."))

  (let ((lake::*tasks* nil)
        (lake::*namespace* nil))
    (is-error (execute "foo")
              simple-error
              "no task.")))


;;
;; Task manager

(subtest "task-exists-p"

  (let ((tasks nil))

    (lake::register-task (lake::make-task "foo" nil nil nil #'noop) tasks)

    (is (lake::task-exists-p "foo" tasks)
        t)

    (is (lake::task-exists-p "bar" tasks)
        nil))

  (is-error (lake::task-exists-p :foo)
            type-error
            "invalid task name.")

  (is-error (lake::task-exists-p "foo" :foo)
            type-error
            "invalid task manager."))

(subtest "get-task"

  (let ((tasks nil)
        (task (lake::make-task "foo" nil nil nil #'noop)))
    (lake::register-task task tasks)
    (is (lake::get-task "foo" tasks)
        task))

  (let ((tasks nil)
        (task1 (lake::make-task "foo" nil nil nil #'noop))
        (task2 (lake::make-task "foo" nil nil nil #'(lambda ()
                                                      (echo "foo")))))
    (lake::register-task task1 tasks)
    (lake::register-task task2 tasks)
    (is-print (lake::%execute-task (lake::get-task "foo" tasks))
              (format nil "foo~%")))

  (is-error (lake::get-task :foo nil)
            type-error
            "invalid task name.")

  (is-error (lake::get-task "foo" nil)
            simple-error
            "no task found.")

  (is-error (lake::get-task "foo" :foo)
            type-error
            "invalid task manager."))


;;
;; lake


(finalize)
