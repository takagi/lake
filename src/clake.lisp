(in-package :cl-user)
(defpackage clake
  (:use :cl
        :annot.doc)
  (:export :clake
           :namespace
           :task
           :file
           :directory
           :sh
           :echo)
  (:shadow :directory)
  (:import-from :alexandria
                :once-only)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :uiop
                :getcwd
                :run-program
                :file-exists-p))
(in-package :clake)

(syntax:use-syntax :annot)


;;;
;;; Utilities
;;;

(defun last1 (list)
  (car (last list)))


;;;
;;; Namespace
;;;

(defvar *namespace* nil)

(defmacro namespace (namespace &body body)
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

(defun task-name-leaf (task-name)
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

(defun task= (task1 task2)
  ;; Now tasks with same names are not permitted.
  (string= (task-name task1)
           (task-name task2)))

(defmethod print-object ((task base-task) stream)
  (format stream "#<TASK ~S>" (task-name task)))

(defmethod task-to-be-executed-p ((task base-task))
  t)

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

(defun make-task (name namespace dependency action)
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace))))
    (make-instance 'task :name name1 :dependency dependency1 :action action)))

(defun dependency-file-name (task-name)
  (task-name-leaf task-name))

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
  ;; Execute the task.
  (format t "~A: " (task-name task))
  (if (task-to-be-executed-p task)
      (progn
        (funcall (task-action task))
        (format t "done.~%"))
      (format t "skipped.~%"))
  (values))

(defmacro task (name dependency &body action)
  `(register-task (make-task ,name *namespace* ',dependency
                             #'(lambda () ,@action))))


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
  (task-name-leaf (task-name file-task)))

(defun file-timestamp (file-name)
  (file-write-date file-name))

(defun file-task-out-of-date (file-task)
  (let ((stamp (file-timestamp (file-task-file-name file-task))))
    (loop for task-name in (task-dependency file-task)
       if (< stamp (file-timestamp (dependency-file-name task-name)))
       return t)))

(defmethod task-to-be-executed-p ((file-task file-task))
  (or (not (file-exists-p (file-task-file-name file-task)))
      (file-task-out-of-date file-task)))

(defmacro file (name dependency &body action)
  `(register-task (make-file-task ,name *namespace* ',dependency
                                  #'(lambda () ,@action))))


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
  (task-name-leaf (task-name directory-task)))

(defun ensure-directory-pathspec (pathspec)
  (if (char/= (aref (reverse pathspec) 0) #\/)
      (concatenate 'string pathspec "/")
      pathspec))

(defmethod execute-task ((directory-task directory-task))
  (format t "~A: " (task-name directory-task))
  (let ((name (ensure-directory-pathspec
               (directory-task-directory-name directory-task))))
    (ensure-directories-exist name))
  (format t "done.~%")
  (values))

(defmacro directory (name)
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


;;;
;;; Task manager
;;;

(defvar *tasks* nil)

(defmacro register-task (task &optional (tasks '*tasks*))
  (once-only (task)
    `(progn
       (check-type ,task base-task)
       (setf ,tasks (remove ,task ,tasks :test #'task=))
       (push ,task ,tasks))))

(defun task-exists-p (name &optional (tasks *tasks*))
  (and (member name tasks :key #'task-name :test #'string=)
       t))

(defun get-task (name &optional (tasks *tasks*))
  (or (car (member name tasks :key #'task-name :test #'string=))
      (error "No task ~S found." name)))


;;;
;;; clake
;;;

(defun get-clakefile-pathname ()
  (or (probe-file (merge-pathnames "Clakefile" (getcwd)))
      (error "No Clakefile found at ~A." (getcwd))))

(defun load-clakefile (pathname)
  (load pathname))

(defun clake (&key (target "default") (pathname (get-clakefile-pathname)))
  (format t "Current directory: ~A~%" (getcwd))
  (let ((*tasks* nil))
    (load-clakefile pathname)
    (%execute-task (get-task target))))
