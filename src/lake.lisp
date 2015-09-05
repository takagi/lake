(in-package :lake)

;;;
;;; lake
;;;

(defun get-lakefile-pathname ()
  (or (probe-file (merge-pathnames "Lakefile" (getcwd)))
      (error "No Lakefile found at ~A." (getcwd))))

(defun load-lakefile (pathname)
  (load pathname))

(defun lake (&key (target "default")
                  (pathname (get-lakefile-pathname))
                  (verbose nil))
  (let ((*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to execute tasks.
    (let ((*tasks* nil))
      (load-lakefile pathname)
      (%execute-task (get-task target)))))

(defun tasks-max-width (tasks)
  (loop for task in tasks
     when (task-description task)
     maximize (length (task-name task))))

(defun %display-tasks (tasks)
  (let ((width (tasks-max-width tasks)))
    (loop for task in tasks
       when (task-description task)
       do (let ((padlen (- width (length (task-name task)))))
            (format t "lake ~A~v@{ ~}  # ~A~%"
                    (task-name task)
                    padlen
                    (task-description task))))))

(defun display-tasks (&key (pathname (get-lakefile-pathname))
                           (verbose nil))
  (let ((*verbose* verbose))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile to display tasks.
    (let ((*tasks*))
      (load-lakefile pathname)
      (%display-tasks (reverse *tasks*)))))
