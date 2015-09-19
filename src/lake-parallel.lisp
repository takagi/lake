(in-package :lake)


;;;
;;; Execution context
;;;

(defclass parallel-context (context) ())

(defun parallel-context ()
  (make-instance 'parallel-context))


;;;
;;; Base task
;;;

(defmethod %execute-task ((task base-task) (context parallel-context))
  (verbose "Execute in parallel." t)
  (call-next-method))


;;;
;;; Task
;;;

;; To avoid curious behavior in *TASKS* propergation across threads.
(defvar *tasks-in-progress*)

(defmethod %execute-task ((task task) (context parallel-context))
  (verbose "Execute in parallel." t)
  (let ((*history* nil)
        (*tasks-in-progress* *tasks*))
    (execute-task task context)))

(defmethod execute-task :before ((task task) (context parallel-context))
  ;; Execute dependency tasks.
  (let ((history (cons task *history*))
        (tasks *tasks-in-progress*)
        (verbose *verbose*))
    (let ((threads
            (mapcar
              #'(lambda (task-name)
                  (bt:make-thread
                    #'(lambda ()
                        (cond
                          ((task-exists-p task-name tasks)
                           (let ((task1 (get-task task-name tasks)))
                             ;; Error if has circular dependency.
                             (unless (not (member task1 history :test #'task=))
                               (error "The task ~S has circular dependency."
                                      (task-name (last1 history))))
                             ;; Execute a dependency task.
                             (let ((*history* history)
                                   (*tasks-in-progress* tasks)
                                   (*verbose* verbose))
                               (execute-task task1 context))))
                          ((file-exists-p (dependency-file-name task-name))
                           ;; Noop.
                           nil)
                          (t (error "Don't know how to build task ~S."
                                    task-name))))
                    :initial-bindings
                    `((*standard-output* . ,*standard-output*)
                      (*error-output* . ,*error-output*))))
              (task-dependency task))))
      (loop for thread in threads
         do (bt:join-thread thread)))))


;;;
;;; lake
;;;

(defun lake (&key (target "default")
                  (pathname (get-lakefile-pathname))
                  parallel
                  verbose)
  (let ((*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to execute tasks.
    (let ((*tasks* nil)
          (context (if parallel
                       (parallel-context)
                       (context))))
      (load-lakefile pathname)
      (%execute-task (get-task target) context))))
