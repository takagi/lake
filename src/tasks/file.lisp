(in-package :lake)

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


