(in-package :lake)

;;;
;;; Verbose
;;;

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

