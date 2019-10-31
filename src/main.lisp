(defpackage #:lake/main
  (:use #:cl)
  (:import-from #:lake/core)
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
          "  -v               Verbose mode.")))


(defun print-tasks (pathname)
  (if pathname
      (lake:display-tasks :pathname pathname)
      (lake:display-tasks)))


(defun main (&rest argv)
  (declare (ignorable argv))
  (let (targets filename jobs f-mode j-mode v-mode)
    (loop for arg in argv
       do (cond
            (f-mode (setf filename arg)
                    (setf f-mode nil))
            (j-mode (setf jobs (parse-integer arg))
                    (setf j-mode nil))
            ((or (string= "-f" arg)
                 (string= "--file" arg))
             (setf f-mode t))
            ((or (string= "-h" arg)
                 (string= "--help" arg))
             (print-help)
             (uiop:quit 1))
            ((string= "-j" arg) (setf j-mode t))
            ((or (string= "-T;" arg)
                 (string= "--list" arg))
             (print-tasks filename)
             (uiop:quit 1))
            ((string= "-v" arg) (setf v-mode t))
            (t (push arg targets))))
    (let ((params `(:verbose ,v-mode
                             ,@(when jobs
                                 `(:jobs ,jobs))
                             ,@(when filename
                                 `(:filename ,filename)))))
      (if targets
          (loop for target in (nreverse targets)
             do (apply #'lake:lake :target target params))
          (apply #'lake:lake params)))))


(defun uiop-main ()
  (handler-bind ((error (lambda (c)
                          (format *error-output* "~A~%"
                                  c)
                          (uiop:quit 1))))
    (apply 'main
           (uiop:command-line-arguments))))
