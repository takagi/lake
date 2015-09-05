(in-package :lake)

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


