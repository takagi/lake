(in-package :cl-user)
(defpackage lake
  (:use :cl)
  (:export :lake
           :display-tasks
           :namespace
           :task
           :file
           :directory
           :echo
           :sh
           :*ssh-host*
           :*ssh-user*
           :*ssh-identity*
           :ssh
           :scp
           :execute
           :getenv
           :*path*)
  (:shadow :directory)
  (:import-from :alexandria
                :once-only)
  (:import-from :split-sequence
                :split-sequence)
  #+thread-support
  (:import-from :lparallel
                :*kernel*
                :make-kernel
                :make-ptree
                :ptree-fn
                :call-ptree
                :task-handler-bind
                :ptree-undefined-function-error
                :invoke-transfer-error)
  (:import-from :uiop
                :getcwd
                :run-program
                :file-exists-p))
(in-package :lake)


;;
;; Utilities

(defun last1 (list)
  (car (last list)))

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
;; FQTN - Fully qualified task name

(defun valid-fqtn-p (string)
  (check-type string string)
  (every #'valid-name-part-p
   (split-sequence #\: string)))

(defun fqtn-namespace (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (format nil "~{~A:~}"
   (butlast
    (split-sequence #\: fqtn))))

(defun fqtn-endname (fqtn)
  (unless (valid-fqtn-p fqtn)
    (error "The value ~S is an invalid FQTN." fqtn))
  (last1 (split-sequence #\: fqtn)))


;;
;; Namespace

(defvar *namespace* nil)

(defmacro namespace (namespace &body body)
  (check-type namespace string)
  `(let ((*namespace* (cons ,namespace *namespace*)))
     ,@body))

(defun valid-task-name-p (task-name)
  (valid-name-part-p task-name))

(defun valid-namespace-p (namespace)
  (every #'valid-name-part-p namespace))

(defun resolve-task-name (task-name namespace)
  (unless (valid-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (format nil "~{~A:~}~A" (reverse namespace) task-name))

(defun valid-dependency-task-name-p (task-name)
  (check-type task-name string)
  (and (string/= task-name "")
       (if (char= #\: (aref task-name 0))
           (every #'valid-name-part-p
            (split-sequence #\: (subseq task-name 1)))
           (every #'valid-name-part-p
            (split-sequence #\: task-name)))))

(defun resolve-dependency-task-name (task-name namespace)
  (unless (valid-dependency-task-name-p task-name)
    (error "The value ~S is an invalid task name." task-name))
  (unless (valid-namespace-p namespace)
    (error "The value ~S is an invalid namespace." namespace))
  (if (char= #\: (aref task-name 0))
      (subseq task-name 1)
      (format nil "~{~A:~}~A" (reverse namespace) task-name)))


;;
;; Task

(defclass task ()
  ((name :initarg :name :reader task-name)
   (dependency :initarg :dependency :reader task-dependency)
   (description :initarg :description :reader task-description)
   (action :initarg :action :reader task-action)))

(defun task= (task1 task2)
  ;; Now tasks with same names are not permitted.
  (string= (task-name task1)
           (task-name task2)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t :identity t)
    (princ (task-name task) stream)))

(defun make-task (name namespace dependency desc action)
  (check-type desc (or string null))
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name namespace)))
        (action1 #'(lambda ()
                     (let ((*namespace* namespace))
                       (funcall action)))))
    (make-instance 'task :name name1
                         :dependency dependency1
                         :description desc
                         :action action1)))

(defun dependency-file-name (task-name)
  (fqtn-endname task-name))

(defgeneric execute-task (task))

(defmethod execute-task ((task task))
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name task)))
  ;; Execute the task.
  (funcall (task-action task))
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

(defvar *tasks* nil)

(defmacro task (name dependency &body body)
  (check-type name string)
  (multiple-value-bind (forms desc) (parse-body body)
    `(register-task (make-task ,name *namespace* ',dependency ,desc
                               #'(lambda ()
                                   ,@forms))
                    *tasks*)))


;;
;; File task

(defclass file-task (task) ())

(defun make-file-task (name namespace dependency desc action)
  (check-type desc (or string null))
  (check-type action function)
  (let ((name1 (resolve-task-name name namespace))
        (dependency1 (loop for task-name in dependency
                        collect
                          (resolve-dependency-task-name task-name
                                                        namespace)))
        (action1 #'(lambda ()
                     (let ((*namespace* namespace))
                       (funcall action)))))
    (make-instance 'file-task :name name1
                              :dependency dependency1
                              :description desc
                              :action action1)))

(defun file-task-file-name (file-task)
  (fqtn-endname (task-name file-task)))

(defun file-timestamp (file-name)
  (file-write-date file-name))

(defun file-task-out-of-date (file-task)
  (let ((stamp (file-timestamp (file-task-file-name file-task))))
    (loop for task-name in (task-dependency file-task)
       if (< stamp (file-timestamp (fqtn-endname task-name)))
       return t)))

(defun file-task-to-be-executed-p (file-task)
  (or (not (file-exists-p (file-task-file-name file-task)))
      (file-task-out-of-date file-task)))

(defmethod execute-task ((file-task file-task))
  ;; Show message if verbose.
  (verbose (format nil "~A: " (task-name file-task)))
  ;; Execute file task if required.
  (if (file-task-to-be-executed-p file-task)
      (progn
        ;; Execute file task.
        (funcall (task-action file-task))
        ;; Show message if verbose.
        (verbose "done." t))
      ;; Skip file task to show message if verbose.
      (verbose "skipped." t))
  (values))

(defmacro file (name dependency &body body)
  (check-type name string)
  (multiple-value-bind (forms desc) (parse-body body)
    `(register-task (make-file-task ,name *namespace* ',dependency ,desc
                                    #'(lambda ()
                                        ,@forms))
                    *tasks*)))


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
        (action1 #'(lambda ()
                     (let ((pathspec
                            (ensure-directory-pathspec name)))
                       (ensure-directories-exist pathspec)))))
    (make-instance 'directory-task :name name1
                                   :dependency nil
                                   :description desc
                                   :action action1)))

(defun directory-task-directory-name (directory-task)
  (check-type directory-task directory-task)
  (fqtn-endname (task-name directory-task)))

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
                           :output t
                           :error-output t
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
;; Execute

(defvar *jobs* 1)

(defun execute (task-name)
  (%execute task-name *namespace* *tasks* *jobs*))

(defun %execute (task-name namespace tasks jobs)
  (let ((task-name1 (resolve-dependency-task-name task-name namespace)))
    (run-task task-name1 tasks jobs)))


;;
;; GETENV

(defun getenv (name &optional default)
  #+cmu
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-cmu
  (or
   #+allegro (sys:getenv name)
   #+clisp (ext:getenv name)
   #+ecl (si:getenv name)
   #+sbcl (sb-unix::posix-getenv name)
   #+lispworks (lispworks:environment-variable name)
   default))

(defun split-by-colon (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\: string :start i)
     collect (subseq string i j)
     while j))


;;
;; *PATH*

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

(defun traverse-tasks (task-name tasks)
  (labels ((%traverse-tasks (task-name tasks history)
             (unless (not (member task-name history :test #'string=))
               (error "The task ~S has circular dependency." task-name))
             (let ((history1 (cons task-name history)))
               (reduce #'(lambda (task-name1 result)
                           (append (%traverse-tasks task-name1 tasks history1)
                                   result))
                       (and (task-exists-p task-name tasks)
                            (task-dependency
                             (get-task task-name tasks)))
                       :from-end t
                       :initial-value (list task-name)))))
    (%traverse-tasks task-name tasks nil)))

#+thread-support
(defun %make-kernel (worker-count)
  ;; Just for binding *DEFAULT-PATHNAME-DEFAULTS*.
  (make-kernel worker-count
               :bindings `((*standard-output* . ,*standard-output*)
                           (*error-output* . ,*error-output*)
                           (*default-pathname-defaults* .
                            ,*default-pathname-defaults*))))

#+thread-support
(defun run-task-concurrent (target tasks jobs)
  (let ((*kernel* (%make-kernel jobs))
        (ptree (make-ptree)))
    ;; Define ptree nodes.
    (dolist (task tasks)
      (let ((task-name (intern (task-name task)))
            (dependency
             (loop for task-name1 in (task-dependency task)
                append
                  (cond
                    ((task-exists-p task-name1 tasks)
                     (list (intern task-name1)))
                    ((file-exists-p
                      (dependency-file-name task-name1))
                     ;; In case the task is not defined but file of the
                     ;; dependency name exists, remove it from CALL-PTREE's
                     ;; dependency computing.
                     nil)
                    (t
                     ;; In case how to build a task is unkown, here append it
                     ;; to dependency list to raise an error on computing
                     ;; dependency in CALL-PTREE below.
                     (list (intern task-name1)))))))
        (ptree-fn task-name dependency
                  (let ((verbose *verbose*)
                        (ssh-host *ssh-host*)
                        (ssh-user *ssh-user*)
                        (ssh-identity *ssh-identity*))
                    #'(lambda (&rest _)
                        (declare (ignore _))
                        (let ((*tasks* tasks) ; for EXECUTE function
                              (*jobs* jobs)   ; for EXECUTE function
                              (*verbose* verbose)
                              (*ssh-host* ssh-host)
                              (*ssh-user* ssh-user)
                              (*ssh-identity* ssh-identity))
                          (execute-task task))))
                  ptree)))
    ;; Call ptree.
    (handler-case
        (task-handler-bind ((error #'invoke-transfer-error))
          (call-ptree (intern target) ptree))
      (ptree-undefined-function-error (e)
        (error "Don't know how to build task ~A."
               (lparallel.ptree::ptree-error-id e))))))

(defun compute-dependency (task-name tasks)
  (remove-duplicates
   (remove nil
    (mapcar #'(lambda (task-name)
                (cond
                  ((task-exists-p task-name tasks)
                   (get-task task-name tasks))
                  ((file-exists-p
                    (dependency-file-name task-name))
                   ;; Noop.
                   nil)
                  (t (error "Don't know how to build task ~S." task-name))))
            (traverse-tasks task-name tasks)))
   :test #'task=
   :from-end t))

(defun run-task-serial (target tasks)
  (let ((*tasks* tasks))                ; for EXECUTE function
    (loop for task in (compute-dependency target tasks)
       do (execute-task task))))

(defun run-task (target tasks &optional (jobs 1))
  #+thread-support
  (if (< 1 jobs)
      (run-task-concurrent target tasks jobs)
      (run-task-serial target tasks))
  #-thread-support
  (run-task-serial target tasks))


;;
;; lake

(defun get-lakefile-pathname ()
  (or (probe-file (merge-pathnames "Lakefile" (getcwd)))
      (error "No Lakefile found at ~A." (getcwd))))

(defun load-lakefile (pathname)
  (load pathname))

(defun lake (&key (target "default")
                  (pathname (get-lakefile-pathname))
                  (jobs 1)
                  (verbose nil))
  (let ((*verbose* verbose)
        (*tasks* nil))
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
