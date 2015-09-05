(in-package :lake)


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


