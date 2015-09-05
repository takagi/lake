(in-package :lake)

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

