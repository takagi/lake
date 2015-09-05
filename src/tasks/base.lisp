(in-package :lake)


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


