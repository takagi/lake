(in-package :cl-user)
(defpackage clake.core
  (:use :cl
        :annot.class
        :annot.doc
        :annot.prove)
  (:import-from :c2mop
                :class-direct-subclasses))
(in-package :clake.core)

(syntax:use-syntax :annot)

@export
@export-class
@tests.around
(let ((obj (make-instance '<clake-base> :args '(:sample))))
  (call-tests))
@tests
((is-type obj
          '<clake-base>
          "can make-instance.")
 (is (args obj)
     '(:sample)
     "can set args."))
@doc
"Base class for Clake classes."
(defclass <clake-base> ()
  ((args :type list
         :initform nil
         :initarg :args
         :reader args)))

@tests
((is-error (call t)
           'simple-error
           "can raise the error with not supported type."))
@export
(defgeneric call (obj)
  (:method ((obj t))
    (error "Not supported type ~a." (type-of obj))))

@tests.around
(let* ((class (defclass <clake-sample> (<clake-base>) ()))
       (symbol (class-name class))
       (string (symbol-name symbol)))
  (unwind-protect
       (progn (call-tests))
    (setf (find-class symbol) nil)))
@tests
((subtest "T"
   (is (find-clake-class symbol)
       class
       "with symbol.")
   (is (find-clake-class string)
       class
       "with string."))
 (subtest "NIL"
   (is (find-clake-class 'not-supported-type)
       nil
       "with symbol.")
   (is (find-clake-class "not-supported-type")
       nil
       "with string.")))
(defun find-clake-class (type)
  (let ((type-name (etypecase type
                     (symbol (symbol-name type))
                     (string type))))
    (loop for class in (class-direct-subclasses (find-class '<clake-base>))
          for class-name = (symbol-name (class-name class))
          when (string-equal class-name type-name)
            do (return-from find-clake-class class))
    nil))

@export
@tests.around
(let* ((class (defclass sample (<clake-base>) ()))
       (class-name (class-name class)))
  (unwind-protect
       (let ((obj (make-clake-obj class-name :arg1 :arg2)))
         (call-tests))
    (setf (find-class class-name) nil)))
@tests
((is-type obj
          'sample
          "can make-instance of the specified type.")
 (is (args obj)
     (list :arg1 :arg2)
     "can set args.")
 (is-error (make-clake-obj :not-supported-type)
           'simple-error
           "can raise the error with not supprted type."))
@doc
"make the specified type obj."
(defun make-clake-obj (type &rest args)
  (let ((class (find-clake-class type)))
    (if class
        (make-instance class :args args)
        (error "Not supported type ~a." type))))
