(in-package :lake)


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

