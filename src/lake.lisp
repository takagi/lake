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


;;;
;;; Utilities
;;;

(defun last1 (list)
  (car (last list)))


;;;
;;; Verbose
;;;

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


;;;
;;; Namespace
;;;

(defvar *namespace* nil)

(defmacro namespace (namespace &body body)
  (check-type namespace string)
  `(let ((*namespace* (cons ,namespace *namespace*)))
     ,@body))

(defun valid-task-name-p (task-name)
  (and (string/= task-name "")
       (not (find #\: task-name))))

(defun valid-namespace-p (namespace)
  (every #'valid-task-name-p namespace))

(defun resolve-task-name (task-name namespace)
  (unless (valid-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (format nil "~{~A:~}~A" (reverse namespace) task-name))

(defun valid-dependency-task-name-p (task-name)
  (and (string/= task-name "")
       (if (char= #\: (aref task-name 0))
           (every #'valid-task-name-p
                  (split-sequence #\: (subseq task-name 1)))
           (every #'valid-task-name-p
                  (split-sequence #\: task-name)))))

(defun resolve-dependency-task-name (task-name namespace)
  (unless (valid-dependency-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (if (char= #\: (aref task-name 0))
      (subseq task-name 1)
      (format nil "~{~A:~}~A" (reverse namespace) task-name)))

(defun task-name-namespace (task-name)
  (cdr (reverse (split-sequence #\: task-name))))

(defun task-name-name (task-name)
  (last1 (split-sequence #\: task-name)))



;;;
;;; Generic task operations
;;;

(defgeneric execute-task (task))


;;;
;;; Base task
;;;

(defclass base-task ()
  ((name :initarg :name :reader task-name)
   (description :initarg :description :reader task-description)))

(defun task-namespace (task)
  (task-name-namespace (task-name task)))

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


;;;
;;; Task
;;;

(defclass task (base-task)
  ((dependency :initarg :dependency :reader task-dependency)
   (action :initarg :action :reader task-action)))

(defun make-task (name namespace dependency desc action)
  (check-type action function)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace))))
    (make-instance 'task :name name1
                         :dependency dependency1
                         :description desc
                         :action action)))

(defun dependency-file-name (task-name)
  (task-name-name task-name))

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
  (let ((*namespace* (task-namespace task)))
    (funcall (task-action task)))
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


;;;
;;; File task
;;;

(defclass file-task (task) ())

(defun make-file-task (name namespace dependency desc action)
  (check-type action function)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace))))
    (make-instance 'file-task :name name1
                              :dependency dependency1
                              :description desc
                              :action action)))

(defun file-task-file-name (file-task)
  (task-name-name (task-name file-task)))

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
        (let ((*namespace* (task-namespace task)))
          (funcall (task-action task)))
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


;;;
;;; Directory task
;;;

(defclass directory-task (base-task) ())

(defun make-directory-task (name namespace desc)
  (check-type desc (or string null))
  (unless (valid-task-name-p name)
    (error "The value ~S is an invalid task name." name))
  (let ((name1 (resolve-task-name name namespace)))
    (make-instance 'directory-task :name name1 :description desc)))

(defun directory-task-directory-name (directory-task)
  (task-name-name (task-name directory-task)))

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


;;;
;;; Echo
;;;

(defun echo (string)
  (write-line string))


;;;
;;; SH
;;;

(defun sh (command &key echo)
  (when echo
    (echo command))
  (run-program command :output t :error-output t))


;;;
;;; SSH
;;;

(defvar *ssh-host*)

(defvar *ssh-user* nil)

(defvar *ssh-identity* nil)

(defparameter +ssh-control-string+
  "ssh ~@[-i ~A ~]-o \"StrictHostKeyChecking no\" ~@[~A@~]~A ~S")

(defun ssh (command &key echo)
  (let ((command1 (format nil +ssh-control-string+
                          *ssh-identity* *ssh-user* *ssh-host* command)))
    (sh command1 :echo echo)))


;;;
;;; Execute
;;;

(defun execute (task-name)
  (%execute task-name *namespace*))

(defun %execute (task-name namespace)
  (let ((task-name1 (resolve-task-name task-name namespace)))
    (%execute-task (get-task task-name1))))


;;;
;;; Task manager
;;;

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


;;;
;;; lake
;;;

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
