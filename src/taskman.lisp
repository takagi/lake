(in-package :lake)

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
