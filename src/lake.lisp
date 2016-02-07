(in-package :cl-user)
(defpackage lake
  (:use :cl)
  (:export :lake
           :display-tasks
           :namespace
           :task
           :file
           :directory
           :echo
           :sh
           :*ssh-host*
           :*ssh-user*
           :*ssh-identity*
           :ssh
           :scp
           :execute
           :getenv
           :*path*)
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


;;
;; Utilities

(defun last1 (list)
  (car (last list)))

(defun valid-name-part-p (string)
  (check-type string string)
  (and (string/= string "")
       (not (find #\: string))))


;;
;; Verbose

(defvar *verbose* nil)

(defun verbose (string &optional new-line (stream *error-output*))
  (check-type string string)
  (check-type stream stream)
  (when *verbose*
    (if new-line
        (write-line string stream)
        (write-string string stream))
    (force-output stream))
  (values))


;;
;; FQTN - Fully qualified task name

(defun valid-fqtn-p (string)
  (check-type string string)
  (every #'valid-name-part-p
   (split-sequence #\: string)))

(defun fqtn-namespace (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (format nil "~{~A:~}"
   (butlast
    (split-sequence #\: fqtn))))

(defun fqtn-endname (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (last1 (split-sequence #\: fqtn)))


;;
;; Namespace

(defvar *namespace* nil)

(defmacro namespace (namespace &body body)
  (check-type namespace string)
  `(let ((*namespace* (cons ,namespace *namespace*)))
     ,@body))

(defun valid-task-name-p (task-name)
  (valid-name-part-p task-name))

(defun valid-namespace-p (namespace)
  (every #'valid-name-part-p namespace))

(defun resolve-task-name (task-name namespace)
  (unless (valid-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (format nil "~{~A:~}~A" (reverse namespace) task-name))

(defun valid-dependency-task-name-p (task-name)
  (check-type task-name string)
  (and (string/= task-name "")
       (if (char= #\: (aref task-name 0))
           (every #'valid-name-part-p
            (split-sequence #\: (subseq task-name 1)))
           (every #'valid-name-part-p
            (split-sequence #\: task-name)))))

(defun resolve-dependency-task-name (task-name namespace)
  (unless (valid-dependency-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (if (char= #\: (aref task-name 0))
      (subseq task-name 1)
      (format nil "~{~A:~}~A" (reverse namespace) task-name)))


;;
;; Generic task operations

(defgeneric execute-task (task))


;;
;; Base task

(defclass base-task ()
  ((name :initarg :name :reader task-name)
   (description :initarg :description :reader task-description)))

(defun task= (task1 task2)
  ;; Now tasks with same names are not permitted.
  (string= (task-name task1)
           (task-name task2)))

(defmethod print-object ((task base-task) stream)
  (print-unreadable-object (task stream :type t :identity t)
    (princ (task-name task) stream)))

(defmethod %execute-task ((task base-task))
  ;; Needed just for (EXECUTE-TASK TASK) because of functional / CLOS sytle
  ;; mismatch.
  (execute-task task))


;;
;; Task

(defclass task (base-task)
  ((dependency :initarg :dependency :reader task-dependency)
   (action :initarg :action :reader task-action)))

(defun make-task (name namespace dependency desc action)
  (check-type action function)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace)))
        (action1 #'(lambda ()
                     (let ((*namespace* namespace))
                       (funcall action)))))
    (make-instance 'task :name name1
                         :dependency dependency1
                         :description desc
                         :action action1)))

(defun dependency-file-name (task-name)
  (fqtn-endname task-name))

(defvar *history*)

(defmethod %execute-task ((task task))
  (let ((*history* nil))
    (execute-task task)))

(defmethod execute-task :before ((task task))
  ;; Execute dependency tasks.
  (let ((*history* (cons task *history*)))
    (loop for task-name in (task-dependency task)
       do (cond
            ((task-exists-p task-name)
             (let ((task1 (get-task task-name)))
               ;; Error if has circular dependency.
               (unless (not (member task1 *history* :test #'task=))
                 (error "The task ~S has circular dependency."
                        (task-name (last1 *history*))))
               ;; Execute a dependency task.
               (execute-task task1)))
            ((file-exists-p (dependency-file-name task-name))
             ;; Noop.
             nil)
            (t (error "Don't know how to build task ~S." task-name))))))

(defmethod execute-task ((task task))
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name task)))
  ;; Execute the task.
  (funcall (task-action task))
  ;; Show message if verbose.
  (verbose "done." t)
  (values))

(defun parse-body (forms)
  (flet ((desc-p (form rest)
           (and (stringp form)
                rest)))
    (if forms
        (destructuring-bind (form1 . rest) forms
            (if (desc-p form1 rest)
                (values rest form1)
                (values forms nil)))
        (values nil nil))))

(defmacro task (name dependency &body body)
  (check-type name string)
  (multiple-value-bind (forms desc) (parse-body body)
    `(register-task (make-task ,name *namespace* ',dependency ,desc
                               #'(lambda ()
                                   ,@forms)))))


;;
;; File task

(defclass file-task (task) ())

(defun make-file-task (name namespace dependency desc action)
  (check-type action function)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace)))
        (action1 #'(lambda ()
                     (let ((*namespace* namespace))
                       (funcall action)))))
    (make-instance 'file-task :name name1
                              :dependency dependency1
                              :description desc
                              :action action1)))

(defun file-task-file-name (file-task)
  (fqtn-endname (task-name file-task)))

(defun file-timestamp (file-name)
  (file-write-date file-name))

(defun file-task-out-of-date (file-task)
  (let ((stamp (file-timestamp (file-task-file-name file-task))))
    (loop for task-name in (task-dependency file-task)
       if (< stamp (file-timestamp (dependency-file-name task-name)))
       return t)))

(defmethod file-task-to-be-executed-p ((file-task file-task))
  (or (not (file-exists-p (file-task-file-name file-task)))
      (file-task-out-of-date file-task)))

(defmethod execute-task ((task file-task))
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name task)))
  ;; Execute the task if required.
  (if (file-task-to-be-executed-p task)
      (progn
        ;; Execute the task.
        (funcall (task-action task))
        ;; Show message if verbose.
        (verbose "done." t))
      ;; Skip the task to show message if verbose.
      (verbose "skipped." t))
  (values))

(defmacro file (name dependency &body body)
  (check-type name string)
  (multiple-value-bind (forms desc) (parse-body body)
    `(register-task (make-file-task ,name *namespace* ',dependency ,desc
                                    #'(lambda ()
                                        ,@forms)))))


;;
;; Directory task

(defclass directory-task (base-task) ())

(defun make-directory-task (name namespace desc)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace)))
    (make-instance 'directory-task :name name1 :description desc)))

(defun directory-task-directory-name (directory-task)
  (fqtn-endname (task-name directory-task)))

(defun ensure-directory-pathspec (pathspec)
  (if (char/= (aref (reverse pathspec) 0) #\/)
      (concatenate 'string pathspec "/")
      pathspec))

(defmethod execute-task ((directory-task directory-task))
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name directory-task)))
  ;; Execute the task.
  (let ((name (ensure-directory-pathspec
               (directory-task-directory-name directory-task))))
    (ensure-directories-exist name))
  ;; Show message if verbose.
  (verbose "done." t)
  (values))

(defmacro directory (name &optional desc)
  (check-type name string)
  (check-type desc (or string null))
  `(register-task (make-directory-task ,name *namespace* ,desc)))


;;
;; Echo

(defun echo (string)
  (write-line string))


;;
;; SH

(defgeneric sh (command &key echo)
   (:documentation "Takes a string or list of strings and runs it from a shell."))

(defmethod sh ((command string) &key echo)
  (when echo
    (echo command))
  (multiple-value-bind (output error-output return-status)
      (run-program command :output t :error-output t :ignore-error-status t)
    (declare (ignore output error-output))
    (unless (zerop return-status)
      (error "Command ~S exited with error code ~A." command return-status))))

(defmethod sh ((command list) &key echo)
  (let ((command1 (format nil "~{~A~^ ~}" command)))
    (sh command1 :echo echo)))


;;
;; SSH

(defvar *ssh-host*)

(defvar *ssh-user* nil)

(defvar *ssh-identity* nil)

(defparameter +ssh-control-string+
  "ssh ~@[-i ~A ~]-o \"StrictHostKeyChecking no\" ~@[~A@~]~A ~S")

(defgeneric ssh (command &key echo)
  (:documentation "Takes a string or list of strings and runs it from a shell on a remote host."))

(defmethod ssh ((command string) &key echo)
  (let ((command1 (format nil +ssh-control-string+
                          *ssh-identity* *ssh-user* *ssh-host* command)))
    (sh command1 :echo echo)))

(defmethod ssh ((command list) &key echo)
  (let ((command1 (format nil "~{~A~^ ~}" command)))
    (ssh command1 :echo echo)))


;;
;; SCP

(defun scp-filepath (pathspec place)
  (check-type pathspec (or pathname string))
  (unless (member place '(:local :remote))
    (error "The value ~S is not :local nor :remote." place))
  (if (eq place :remote)
      (format nil "~@[~A@~]~A:~A" *ssh-user* *ssh-host* pathspec)
      (princ-to-string pathspec)))

(defparameter +scp-control-string+
  "scp ~@[-i ~A ~]-o \"StrictHostKeyChecking no\" ~A ~A")

(defun scp (from-place pathspec1 to-place pathspec2 &key echo)
  (let ((path1 (scp-filepath pathspec1 from-place))
        (path2 (scp-filepath pathspec2 to-place)))
    (let ((command (format nil +scp-control-string+
                           *ssh-identity* path1 path2)))
      (sh command :echo echo))))


;;
;; Execute

(defun execute (task-name)
  (%execute task-name *namespace*))

(defun %execute (task-name namespace)
  (let ((task-name1 (resolve-dependency-task-name task-name namespace)))
    (%execute-task (get-task task-name1))))


;;
;; GETENV

(defun getenv (name &optional default)
  #+cmu
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-cmu
  (or
   #+allegro (sys:getenv name)
   #+clisp (ext:getenv name)
   #+ecl (si:getenv name)
   #+sbcl (sb-unix::posix-getenv name)
   #+lispworks (lispworks:environment-variable name)
   default))

(defun split-by-colon (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\: string :start i)
     collect (subseq string i j)
     while j))


;;
;; *PATH*

(defvar *path*
  (mapcar #'cl:directory (split-by-colon (getenv "PATH"))))


;;
;; Task manager

(defvar *tasks* nil)

(defmacro register-task (task &optional (tasks '*tasks*))
  (once-only (task)
    `(progn
       (check-type ,task base-task)
       (setf ,tasks (remove-duplicates ,tasks :test #'task=))
       (push ,task ,tasks))))

(defun task-exists-p (name &optional (tasks *tasks*))
  (check-type name string)
  (and (member name tasks :key #'task-name :test #'string=)
       t))

(defun get-task (name &optional (tasks *tasks*))
  (check-type name string)
  (or (car (member name tasks :key #'task-name :test #'string=))
      (error "No task ~S found." name)))


;;
;; lake

(defun get-lakefile-pathname ()
  (or (probe-file (merge-pathnames "Lakefile" (getcwd)))
      (error "No Lakefile found at ~A." (getcwd))))

(defun load-lakefile (pathname)
  (load pathname))

(defun lake (&key (target "default")
                  (pathname (get-lakefile-pathname))
                  (verbose nil))
  (let ((*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to execute tasks.
    (let ((*tasks* nil))
      (load-lakefile pathname)
      (%execute-task (get-task target)))))

(defun tasks-max-width (tasks)
  (loop for task in tasks
     when (task-description task)
     maximize (length (task-name task))))

(defun %display-tasks (tasks)
  (let ((width (tasks-max-width tasks)))
    (loop for task in tasks
       when (task-description task)
       do (let ((padlen (- width (length (task-name task)))))
            (format t "lake ~A~v@{ ~}  # ~A~%"
                    (task-name task)
                    padlen
                    (task-description task))))))

(defun display-tasks (&key (pathname (get-lakefile-pathname))
                           (verbose nil))
  (let ((*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to display tasks.
    (let ((*tasks*))
      (load-lakefile pathname)
      (%display-tasks (reverse *tasks*)))))
