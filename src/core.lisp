(defpackage lake
  (:use :cl)
  (:nicknames #:lake/core)
  (:export #:lake
           #:display-tasks
           #:execute
           #:namespace
           #:task
           #:file
           #:directory
           #:echo
           #:sh
           #:*ssh-host*
           #:*ssh-user*
           #:*ssh-identity*
           #:ssh
           #:scp
           #:getenv
           #:*path*)
  (:shadow #:directory)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:ensure-list
                #:once-only)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:uiop
                #:getcwd
                #:file-exists-p
                #:run-program)
  #+thread-support
  (:import-from #:lparallel
                #:*kernel*
                #:make-kernel
                #:make-ptree
                #:ptree-fn
                #:call-ptree
                #:task-handler-bind
                #:ptree-undefined-function-error
                #:invoke-transfer-error))
(in-package lake)


;;
;; Utilities

(defun ensure-pair (x)
  (if (listp x)
      (or (and (cdr x) (null (cddr x)) x)
          (error "The value ~S is invalid pair." x))
      (list x nil)))

(defun valid-name-part-p (string)
  (check-type string string)
  (and (string/= string "")
       (not (find #\: string))))


;;
;; Verbose

(defvar *verbose* nil)

(defun verbose (string &optional new-line (stream *error-output*))
  (check-type string string)
  (check-type stream stream)
  (when *verbose*
    (if new-line
        (write-line string stream)
        (write-string string stream))
    (force-output stream))
  (values))


;;
;; FQTN - Fully qualified task names

(defun valid-fqtn-p (string)
  (check-type string string)
  (every #'valid-name-part-p
   (split-sequence #\: string)))

(defun fqtn-namespace (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (cdr
   (nreverse
    (split-sequence #\: fqtn))))

(defun fqtn-endname (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (car
   (nreverse
    (split-sequence #\: fqtn))))


;;
;; Namespace

(defun valid-task-name-p (task-name)
  (valid-name-part-p task-name))

(defun valid-namespace-p (namespace)
  (every #'valid-name-part-p namespace))

(defun valid-dependency-task-name-p (name)
  (check-type name string)
  (and (string/= name "")
       (if (char= #\: (aref name 0))
           (every #'valid-name-part-p
            (split-sequence #\: (subseq name 1)))
           (every #'valid-name-part-p
            (split-sequence #\: name)))))

(defun resolve-task-name (name namespace)
  (unless (valid-task-name-p name)
    (error "The value ~S is an invalid task name." name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (format nil "~{~A:~}~A" (reverse namespace) name))

(defun resolve-dependency-task-name (name namespace)
  (unless (valid-dependency-task-name-p name)
    (error "The value ~S is an invalid task name." name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (if (char= #\: (aref name 0))
      (subseq name 1)
      (format nil "~{~A:~}~A" (reverse namespace) name)))

(defun resolve-dependency-task-names (dependency namespace)
  (loop for name in dependency
     collect (resolve-dependency-task-name name namespace)))


;;
;; Task

(defclass task ()
  ((name :initarg :name :reader task-name)
   (namespace :initarg :namespace :reader task-namespace)
   (arguments :initarg :arguments :reader task-arguments)
   (dependency :initarg :dependency :reader task-dependency)
   (description :initarg :description :reader task-description)
   (action :initarg :action :reader task-action)))

(defun arg-pair-p (x)
  (and (listp x)
       (symbolp (car x))
       (cdr x)
       (null (cddr x))))

(defun argument-p (x)
  (or (symbolp x)
      (arg-pair-p x)))

(deftype argument ()
  '(satisfies argument-p))

(defun make-task (name namespace args dependency desc action)
  (dolist (arg args)
    (check-type arg argument))
  (check-type desc (or string null))
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (resolve-dependency-task-names dependency namespace)))
    (make-instance 'task :name name1
                         :namespace namespace
                         :arguments args
                         :dependency dependency1
                         :description desc
                         :action action)))

(defun task= (task1 task2)
  ;; Now tasks with the same name are not permitted.
  (string= (task-name task1)
           (task-name task2)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t :identity nil)
    (princ (task-name task) stream)))

(defun dependency-file-name (task-name)
  (fqtn-endname task-name))

(defgeneric execute-task (task &optional args))

(defmethod execute-task ((task task) &optional args)
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name task)))
  ;; Execute the task.
  (apply (task-action task) args)
  ;; Show message if verbose.
  (verbose "done." t)
  (values))


;;
;; File task

(defclass file-task (task) ())

(defun make-file-task (name namespace args dependency desc action)
  (dolist (arg args)
    (check-type arg argument))
  (check-type desc (or string null))
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (resolve-dependency-task-names dependency namespace)))
    (make-instance 'file-task :name name1
                              :namespace namespace
                              :arguments args
                              :dependency dependency1
                              :description desc
                              :action action)))

(defun file-task-file-name (file-task)
  (fqtn-endname (task-name file-task)))

(defun file-timestamp (file-name)
  (file-write-date file-name))

(defun file-task-out-of-date (file-task)
  (let ((stamp (file-timestamp (file-task-file-name file-task))))
    (loop for name in (task-dependency file-task)
       if (< stamp (file-timestamp (fqtn-endname name)))
       return t)))

(defun file-task-to-be-executed-p (file-task)
  (or (not (file-exists-p (file-task-file-name file-task)))
      (file-task-out-of-date file-task)))

(defmethod execute-task ((file-task file-task) &optional args)
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name file-task)))
  ;; Execute file task if required.
  (if (file-task-to-be-executed-p file-task)
      (progn
        ;; Execute file task.
        (apply (task-action file-task) args)
        ;; Show message if verbose.
        (verbose "done." t))
      ;; Skip file task to show message if verbose.
      (verbose "skipped." t))
  (values))


;;
;; Directory task

(defclass directory-task (task) ())

(defun ensure-directory-pathspec (pathspec)
  (if (char/= (aref (reverse pathspec) 0) #\/)
      (concatenate 'string pathspec "/")
      pathspec))

(defun make-directory-task (name namespace desc)
  (check-type desc (or string null))
  (let ((name1 (resolve-task-name name namespace))
        (action #'(lambda ()
                    (let ((pathspec
                           (ensure-directory-pathspec name)))
                      (ensure-directories-exist pathspec)))))
    (make-instance 'directory-task :name name1
                                   :namespace nil
                                   :arguments nil
                                   :dependency nil
                                   :description desc
                                   :action action)))

(defun directory-task-directory-name (directory-task)
  (check-type directory-task directory-task)
  (fqtn-endname (task-name directory-task)))


;;
;; Macros

(defvar *tasks* nil)

(defvar *namespace* nil)

(defmacro namespace (namespace &body body)
  (check-type namespace string)
  `(let ((*namespace* (cons ,namespace *namespace*)))
     ,@body))

(defun name-and-args-p (x)
  (and (listp x)
       (stringp (car x))
       (every #'argument-p (cdr x))))

(deftype name-and-args ()
  '(satisfies name-and-args-p))

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

(defmacro task (name-and-args dependency &body body)
  (check-type name-and-args (or string name-and-args))
  (destructuring-bind (name . args) (ensure-list name-and-args)
    (let ((args1 (mapcar #'car
                  (mapcar #'ensure-pair args))))
      (multiple-value-bind (forms desc) (parse-body body)
        `(register-task (make-task ,name *namespace* ',args ',dependency ,desc
                                   #'(lambda ,args1
                                       ,@forms))
                        *tasks*)))))

(defmacro file (name-and-args dependency &body body)
  (check-type name-and-args (or string name-and-args))
  (destructuring-bind (name . args) (ensure-list name-and-args)
    (let ((args1 (mapcar #'car
                  (mapcar #'ensure-pair args))))
      (multiple-value-bind (forms desc) (parse-body body)
        `(register-task
          (make-file-task ,name *namespace* ',args ',dependency ,desc
                          #'(lambda ,args1
                              ,@forms))
          *tasks*)))))

(defmacro directory (name &optional desc)
  (check-type name string)
  (check-type desc (or string null))
  `(register-task (make-directory-task ,name *namespace* ,desc) *tasks*))


;;
;; Echo

#+thread-support
(let ((lock (bt:make-lock)))
  (defun echo (string)
    (bt:with-lock-held (lock)
      (write-line string))))

#-thread-support
(defun echo (string)
  (write-line string))


;;
;; SH

(defgeneric sh (command &key echo)
   (:documentation
    "Takes a string or list of strings and runs it from a shell."))

(defmethod sh ((command string) &key echo)
  (when echo
    (echo command))
  (multiple-value-bind (output error-output return-status)
      (run-program command :input :interactive
                           :output :interactive
                           :error-output :interactive
                           :ignore-error-status t)
    (declare (ignore output error-output))
    (unless (zerop return-status)
      (error "Command ~S exited with error code ~A." command return-status))))

(defmethod sh ((command list) &key echo)
  (let ((command1 (format nil "~{~A~^ ~}" command)))
    (sh command1 :echo echo)))


;;
;; SSH

(defvar *ssh-host* nil)

(defvar *ssh-user* nil)

(defvar *ssh-identity* nil)

(defun escape-for-shell (string)
  (flet ((regex-replace-all (regex replacement target-string)
           (cl-ppcre:regex-replace-all regex target-string replacement)))
    (regex-replace-all "\\$" "\\$"
     (regex-replace-all "\"" "\\\""
      (regex-replace-all "\\" "\\\\\\\\" string)))))

(defparameter +ssh-control-string+
  "ssh -q -t ~@[-i ~A ~]-o \"StrictHostKeyChecking no\" ~@[~A@~]~A \"~A\"")

(defgeneric ssh (command &key echo)
  (:documentation "Takes a string or list of strings and runs it from a shell on a remote host."))

(defmethod ssh ((command string) &key echo)
  (unless *ssh-host*
    (error "*SSH-HOST* is not specified."))
  (let* ((command1 (format nil "/bin/bash -l -c \"~A\""
                    (escape-for-shell command)))
         (command2 (format nil +ssh-control-string+
                           *ssh-identity* *ssh-user* *ssh-host*
                    (escape-for-shell command1))))
    (sh command2 :echo echo)))

(defmethod ssh ((command list) &key echo)
  (let ((command1 (format nil "~{~A~^ ~}" command)))
    (ssh command1 :echo echo)))


;;
;; SCP

(defun scp-filepath (pathspec place)
  (check-type pathspec (or pathname string))
  (unless (member place '(:local :remote))
    (error "The value ~S is not :local nor :remote." place))
  (unless *ssh-host*
    (error "*SSH-HOST* is not specified."))
  (if (eq place :remote)
      (format nil "~@[~A@~]~A:~A" *ssh-user* *ssh-host* pathspec)
      (princ-to-string pathspec)))

(defparameter +scp-control-string+
  "scp ~@[-i ~A ~]-r -o \"StrictHostKeyChecking no\" ~A ~A")

(defun scp (from-place pathspec1 to-place pathspec2 &key echo)
  (let ((path1 (scp-filepath pathspec1 from-place))
        (path2 (scp-filepath pathspec2 to-place)))
    (let ((command (format nil +scp-control-string+
                           *ssh-identity* path1 path2)))
      (sh command :echo echo))))


;;
;; GETENV

(defun getenv (name &optional default)
  (or (uiop:getenv name)
      default))


;;
;; *PATH*

(defun split-by-colon (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\: string :start i)
     collect (subseq string i j)
     while j))

(defvar *path*
  (mapcar #'cl:directory (split-by-colon (getenv "PATH"))))


;;
;; Task manager

(defmacro register-task (task tasks)
  (once-only (task)
    `(progn
       (check-type ,task task)
       (setf ,tasks (remove ,task ,tasks :test #'task=))
       (push ,task ,tasks))))

(defun task-exists-p (name tasks)
  (check-type name string)
  (and (member name tasks :key #'task-name :test #'string=)
       t))

(defun get-task (name tasks)
  (check-type name string)
  (or (car (member name tasks :key #'task-name :test #'string=))
      (error "No task ~S found." name)))

(defvar *context-tasks*)

(defvar *context-namespace*)

(defvar *context-plist*)

(defvar *context-jobs*)

(defun get-environment-variable (name)
  (check-type name string)
  (uiop:getenv (string-upcase name)))

(defun maybe (fn x)
  (and x
       (funcall fn x)))

(defun read-argument-from-string (string)
  (check-type string string)
  (cond
    ((string= "T" (string-upcase string)) t)
    ((string= "NIL" (string-upcase string)) nil)
    (t string)))

(defun get-task-arguments (task plist)
  (check-type task task)
  (check-type plist list)
  (let ((task-args (mapcar #'ensure-pair
                    (task-arguments task))))
    (loop for (symbol default) in task-args
       collect
         (or (getf plist symbol)
             (maybe #'read-argument-from-string
              (get-environment-variable (symbol-name symbol)))
             default
             nil))))

#+thread-support
(defun %make-kernel (worker-count)
  ;; Just for binding *DEFAULT-PATHNAME-DEFAULTS*.
  (make-kernel worker-count
               :bindings `((*standard-output* . ,*standard-output*)
                           (*error-output* . ,*error-output*)
                           (*default-pathname-defaults* .
                            ,*default-pathname-defaults*))))

#+thread-support
(defun run-task-concurrent (target tasks plist jobs)
  (let ((*kernel* (%make-kernel jobs))
        (ptree (make-ptree)))
    ;; Define ptree nodes.
    (dolist (task tasks)
      (let ((name (intern (task-name task)))
            (dependency
             (loop for name1 in (task-dependency task)
                append
                  (cond
                    ((task-exists-p name1 tasks)
                     (list (intern name1)))
                    ((file-exists-p
                      (dependency-file-name name1))
                     ;; In case that the task is not defined but file of the
                     ;; dependency name exists, remove it from CALL-PTREE's
                     ;; dependency computing.
                     nil)
                    (t
                     ;; In case that how to build a task is unkown, here append
                     ;; it to dependency list to raise an error on computing
                     ;; dependency in CALL-PTREE below.
                     (list (intern name1)))))))
        (ptree-fn name dependency
                  (let ((namespace (task-namespace task))
                        (verbose *verbose*)
                        (ssh-host *ssh-host*)
                        (ssh-user *ssh-user*)
                        (ssh-identity *ssh-identity*))
                    #'(lambda (&rest _)
                        (declare (ignore _))
                        (let ((*context-tasks* tasks)
                              (*context-plist* plist)
                              (*context-namespace* namespace)
                              (*context-jobs* jobs)
                              (*verbose* verbose)
                              (*ssh-host* ssh-host)
                              (*ssh-user* ssh-user)
                              (*ssh-identity* ssh-identity)
                              (args (get-task-arguments task plist)))
                          (execute-task task args))))
                  ptree)))
    ;; Call ptree.
    (handler-case
        (task-handler-bind ((error #'invoke-transfer-error))
          (call-ptree (intern target) ptree))
      (ptree-undefined-function-error (e)
        (error "Don't know how to build task ~A."
               (lparallel.ptree::ptree-error-id e))))))

(defun traverse-tasks (name tasks)
  (labels ((%traverse-tasks (name tasks history)
             (unless (not (member name history :test #'string=))
               (error "The task ~S has circular dependency." name))
             (let ((history1 (cons name history)))
               (reduce #'(lambda (name1 result)
                           (append (%traverse-tasks name1 tasks history1)
                                   result))
                       (and (task-exists-p name tasks)
                            (task-dependency
                             (get-task name tasks)))
                       :from-end t
                       :initial-value (list name)))))
    (%traverse-tasks name tasks nil)))

(defun compute-dependency (name tasks)
  (remove-duplicates
   (remove nil
    (mapcar #'(lambda (name)
                (cond
                  ((task-exists-p name tasks)
                   (get-task name tasks))
                  ((file-exists-p
                    (dependency-file-name name))
                   ;; Noop.
                   nil)
                  (t (error "Don't know how to build task ~S." name))))
            (traverse-tasks name tasks)))
   :test #'task=
   :from-end t))

(defun run-task-serial (target tasks plist)
  (let ((*context-tasks* tasks)
        (*context-plist* plist)
        (*context-jobs* 1))
    (loop for task in (compute-dependency target tasks)
       do (let ((*context-namespace* (task-namespace task))
                (args (get-task-arguments task plist)))
            (execute-task task args)))))

(defun %run-task (target tasks plist jobs)
  #-thread-support
  (declare (ignore jobs))
  #+thread-support
  (if (< 1 jobs)
      (run-task-concurrent target tasks plist jobs)
      (run-task-serial target tasks plist))
  #-thread-support
  (run-task-serial target tasks plist))

(defun parse-args (string result)
  (cl-ppcre:register-groups-bind (arg remaining-args)
      ("((?:[^\\\\,]|\\\\.)*?)\\s*(?:,\\s*(.*))?$" string :sharedp t)
    (let* ((arg1 (read-argument-from-string
                  (cl-ppcre:regex-replace-all "\\\\(.)" arg "\\1")))
           (result1 (cons arg1 result)))
      (if remaining-args
          (parse-args remaining-args result1)
          result1))))

(defun parse-target (string)
  (multiple-value-bind (name args)
      (cl-ppcre:register-groups-bind (name remaining-args)
          ("^([^\\[]+)\\[(.*)\\]$" string :sharedp t)
        (if (and remaining-args
                 (string/= remaining-args ""))
            (values name (nreverse (parse-args remaining-args nil)))
            (values name nil)))
    (if name
        (values name args)
        (values string nil))))

(defun construct-plist (name args tasks)
  (let* ((task (get-task name tasks))
         (task-args (mapcar #'car
                     (mapcar #'ensure-pair
                      (task-arguments task)))))
    (loop for arg in args
          for task-arg in task-args
       append (list task-arg arg))))

(defun run-task (target tasks &optional (jobs 1))
  (let* ((*package* (find-package :lake/user)))
    (multiple-value-bind (name args) (parse-target target)
      (let ((plist (construct-plist name args tasks)))
        (%run-task name tasks plist jobs)))))

(defun execute (name)
  (let ((name1 (resolve-dependency-task-name name *context-namespace*)))
    (%run-task name1 *context-tasks* *context-plist* *context-jobs*)))


(defun prompt (message &key allowed-answers)
  (format *query-io* "~A~%" message)
  (finish-output *query-io*)
  (let ((answer (read-line *query-io*)))
    (cond
      ((and allowed-answers
            (not (member answer allowed-answers
                         :test #'string=)))
       (format *query-io*
               "Answer should be ~{~A~#[~; or ~:;, ~]~}~2%"
               allowed-answers)
       (prompt message :allowed-answers allowed-answers))
      (t answer))))

;;
;; Lake

(defparameter +lakefile-template+
  '((task "greet" ()
      (sh "echo \"Hello world!\""))
    (task "default" ("greet")
      (echo "This was default task."))))

(defun initialize-lakefile (path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :error
                       :if-does-not-exist :create)
    (write-line "#|-*- mode:lisp -*-|#" out)
    (loop for form in +lakefile-template+
          do (format out "~%")
             (write form
                    :stream out
                    :pretty t
                    :case :downcase)
             (format out "~%"))))

(defun get-lakefile-pathname (&optional (name "Lakefile"))
  (let* ((path (merge-pathnames name (getcwd)))
         (probed-path (probe-file path)))
    (cond
      (probed-path probed-path)
      (t (format *query-io*
                 "~A not found at ~A.~%"
                 name
                 (getcwd))
         (let ((response (prompt "Do you want to create it? [yes/no]"
                                 :allowed-answers '("yes" "no"))))
           (cond
             ((string= response
                       "yes")
              (initialize-lakefile path)
              (error "~A was created. Now edit it and run lake again."
                     path))
             (t (error "You need to create a ~A"
                       path))))))))

(defun load-lakefile (pathname)
  (let* ((*package* (find-package :lake/user)))
    (cl-syntax:use-syntax :interpol)
    (load pathname)))

(defun lake (&key (target "default")
                  (filename "Lakefile")
                  (jobs 1)
                  (verbose nil))
  (let ((*verbose* verbose)
        (*tasks* nil)
        (pathname (get-lakefile-pathname filename)))
    ;; Show message if verbose.
    (verbose (format nil "Current directory: ~A~%" (getcwd)))
    ;; Load Lakefile.
    (load-lakefile pathname)
    ;; Execute target task.
    (run-task target *tasks* jobs)))

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
