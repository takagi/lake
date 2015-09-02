(in-package :cl-user)
(defpackage lake
  (:use :cl)
  (:export :lake
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


;;;
;;; Utilities
;;;

(defun last1 (list)
  (car (last list)))

(defmacro let% (bindings &body body)
  (if bindings
      (destructuring-bind ((var value) . rest) bindings
        (alexandria:with-gensyms (orig)
          `(let ((,orig ,var))
             (setf ,var ,value)
             (unwind-protect
                  (let% (,@rest)
                    ,@body)
               (setf ,var ,orig)))))
      `(progn ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun dolist%-sequential-form (var list body)
    `(dolist (,var ,list)
       ,@body))

  (defun dolist%-parallel-form (var list body)
    `(let ((threads
             (mapcar
               #'(lambda (,var)
                   (let ((parent (bt:current-thread)))
                     (bt:make-thread
                       #'(lambda ()
                           (handler-case
                               (progn ,@body)
                             (error (e)
                               (bt:interrupt-thread parent
                                 #'(lambda ()
                                     (error e))))))
                       :initial-bindings
                       `((*standard-output* . ,*standard-output*)
                         (*error-output* . ,*error-output*)))))
               ,list)))
       (loop for thread in threads
          do (bt:join-thread thread)))))

(defmacro dolist% ((var list parallel) &body body)
  (cond
    ((eq parallel t) (dolist%-parallel-form var list body))
    ((eq parallel nil) (dolist%-sequential-form var list body))
    (t (once-only (list)
         `(if (not ,parallel)
              ,(dolist%-sequential-form var list body)
              ,(dolist%-parallel-form var list body))))))


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
  ((name :initarg :name :reader task-name)))

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
  (when *parallel*
    (verbose "Execute in parallel." t))
  (execute-task task))


;;;
;;; Task
;;;

(defclass task (base-task)
  ((dependency :initarg :dependency :reader task-dependency)
   (action :initarg :action :reader task-action)))

(defun make-task (name namespace dependency action)
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace))))
    (make-instance 'task :name name1 :dependency dependency1 :action action)))

(defun dependency-file-name (task-name)
  (task-name-name task-name))

(defvar *history*)

(defmethod %execute-task ((task task))
  (when *parallel*
    (verbose "Execute in parallel." t))
  (let ((*history* nil))
    (execute-task task)))

(defvar *parallel* nil)

(defmethod execute-task :before ((task task))
  ;; Execute dependency tasks.
  (let ((history (cons task *history*))
        (tasks *tasks*))
    (dolist% (task-name (task-dependency task) *parallel*)
      (cond
        ((task-exists-p task-name tasks)
         (let ((task1 (get-task task-name tasks)))
           ;; Error if has circular dependency.
           (unless (not (member task1 history :test #'task=))
             (error "The task ~S has circular dependency."
                    (task-name (last1 history))))
           ;; Execute a dependency task.
           (let ((*history* history)
                 (*tasks* tasks))
             (execute-task task1))))
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

(defmacro task (name dependency &body action)
  (check-type name string)
  `(register-task (make-task ,name *namespace* ',dependency
                             #'(lambda ()
                                 ,@action))))


;;;
;;; File task
;;;

(defclass file-task (task) ())

(defun make-file-task (name namespace dependency action)
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace))))
    (make-instance 'file-task
                   :name name1
                   :dependency dependency1
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

(defmacro file (name dependency &body action)
  (check-type name string)
  `(register-task (make-file-task ,name *namespace* ',dependency
                                  #'(lambda ()
                                      ,@action))))


;;;
;;; Directory task
;;;

(defclass directory-task (base-task) ())

(defun make-directory-task (name namespace)
  (unless (valid-task-name-p name)
    (error "The value ~S is an invalid task name." name))
  (let ((name1 (resolve-task-name name namespace)))
    (make-instance 'directory-task :name name1)))

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

(defmacro directory (name)
  (check-type name string)
  `(register-task (make-directory-task ,name *namespace*)))


;;;
;;; Run
;;;

(defun echo (string)
  (write-line string))

(defun sh (command &key echo)
  (when echo
    (echo command))
  (run-program command :output t :error-output t))

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
                  parallel
                  verbose)
  (let% ((*parallel* parallel)
         (*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to execute tasks.
    (let ((*tasks* nil))
      (load-lakefile pathname)
      (%execute-task (get-task target)))))
