(defpackage #:lake/main
  (:use #:cl)
  (:import-from #:lake/user)
  (:export #:main
           #:uiop-main))
(in-package lake/main)


(defun print-help ()
  (mapc #'write-line
        '("Usage: lake [options] [target] ..."
          "Options:"
          "  --file, -f FILE  Use FILE as a lakefile."
          "  --help, -h       Print this message and exit."
          "  -j INTEGER       Execute multiple tasks simultaneously."
          "  --list, -T       Display the tasks with descriptions, then exit."
          "  -v, --verbose    Verbose mode.")))


(defun print-tasks (pathname &key verbose)
  (if pathname
      (lake:display-tasks :pathname pathname
                          :verbose verbose)
      (lake:display-tasks :verbose verbose)))


(defun main (&rest argv)
  (declare (ignorable argv))
  (let (targets f-mode j-mode
        (params (list :verbose nil
                      :filename "Lakefile"
                      :jobs 1)))
    (loop for arg in argv
          do (cond
               (f-mode (setf (getf params :filename) arg)
                       (setf f-mode nil))
               (j-mode (setf (getf params :jobs)
                             (parse-integer arg))
                       (setf j-mode nil))
               ((or (string= "-f" arg)
                    (string= "--file" arg))
                (setf f-mode t))
               ((or (string= "-h" arg)
                    (string= "--help" arg))
                (print-help)
                (uiop:quit 1))
               ((string= "-j" arg)
                (setf j-mode t))
               ((or (string= "-T" arg)
                    (string= "--list" arg))
                (print-tasks (getf params :filename)
                             :verbose (getf params :verbose))
                (uiop:quit 1))
               ((or (string= "-v" arg)
                    (string= "--verbose" arg))
                (setf (getf params :verbose)
                      t))
               (t (push arg targets))))
    (if targets
        (loop for target in (nreverse targets)
              do (apply #'lake:lake :target target params))
        (apply #'lake:lake params))))


(defun uiop-main ()
  (handler-bind ((error (lambda (c)
                          (format *error-output* "~A~%"
                                  c)
                          (uiop:quit 1))))
    (apply 'main
           (uiop:command-line-arguments))))
