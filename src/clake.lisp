(in-package :cl-user)
(defpackage clake
  (:use :cl
        :annot.doc)
  (:export :task
           :file
           :directory
           :sh)
  (:shadow :directory)
  (:import-from :alexandria
                :once-only)
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

(defmethod execute-task :before ((task base-task))
  (format t "~A: " (task-name task)))

(defmethod execute-task :after ((task base-task))
  (format t "done.~%"))


;;;
;;; Task
;;;

(defclass task (base-task)
  ((dependency :initarg :dependency :reader task-dependency)
   (action :initarg :action :reader task-action)))

(defun make-task (name dependency action)
  (check-type name string)
  (dolist (name1 dependency)
    (check-type name1 string))
  (check-type action function)
  (make-instance 'task :name name :dependency dependency :action action))

(defun task-dependency-tasks (task)
  (loop for name in (task-dependency task)
     if (task-exists-p name)
     collect (get-task name)))

(defvar *history*)

(defmethod %execute-task ((task task))
  (let ((*history* nil))
    (execute-task task)))

(defmethod execute-task :before ((task task))
  ;; Execute dependency tasks.
  (let ((*history* (cons task *history*)))
    (loop for task1 in (task-dependency-tasks task)
       do ;; Error if has circular dependency.
          (unless (not (member task1 *history* :test #'task=))
            (error "The task ~S has circular dependency." (task-name
                                                           (last1 *history*))))
          ;; Execute a dependency task.
          (execute-task task1))))

(defmethod execute-task ((task task))
  ;; Execute the task.
  (funcall (task-action task)))

(defmethod execute-task :around ((task task))
  ;; Skip if not needed.
  (if (task-to-be-executed-p task)
      (call-next-method)
      (format t "~A: skipped.~%" (task-name task))))

(defmacro task (name dependency &body action)
  `(register-task (make-task ,name ',dependency #'(lambda () ,@action))))


;;;
;;; File task
;;;

(defclass file-task (task) ())

(defun make-file-task (name dependency action)
  (check-type name string)
  (dolist (name1 dependency)
    (check-type name1 string))
  (check-type action function)
  (make-instance 'file-task :name name :dependency dependency :action action))

(defun file-timestamp (file-name)
  (file-write-date file-name))

(defun file-task-out-of-date (file-task)
  (let ((stamp (file-timestamp (task-name file-task))))
    (loop for name in (task-dependency file-task)
       if (< stamp (file-timestamp name))
       return t)))

(defmethod task-to-be-executed-p ((file-task file-task))
  (or (not (file-exists-p (task-name file-task)))
      (file-task-out-of-date file-task)))

(defmacro file (name dependency &body action)
  `(register-task (make-file-task ,name ',dependency #'(lambda () ,@action))))


;;;
;;; Directory task
;;;

(defclass directory-task (base-task) ())

(defun make-directory-task (name)
  (check-type name string)
  (make-instance 'directory-task :name name))

(defun ensure-directory-pathspec (pathspec)
  (if (char/= (aref (reverse pathspec) 0) #\/)
      (concatenate 'string pathspec "/")
      pathspec))

(defmethod execute-task ((directory-task directory-task))
  (let ((name (ensure-directory-pathspec
               (task-name directory-task))))
    (ensure-directories-exist name)))

(defmacro directory (name)
  `(register-task (make-directory-task ,name)))


;;;
;;; Run
;;;

(defun sh (command)
  (run-program command :ignore-error-status t :output t :error-output t))


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

(defun execute-tasks (&optional (tasks *tasks*))
  (dolist (task (reverse tasks))
    (%execute-task task)))

(defun clake (&key (target "default") (pathname (get-clakefile-pathname)))
  (format t "Current directory: ~A~%" (getcwd))
  (let ((*tasks* nil))
    (load-clakefile pathname)
    (%execute-task (get-task target))))
