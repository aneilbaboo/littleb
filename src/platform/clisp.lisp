(in-package :cl-user)

(port:allowing-redefinitions ()
  (cl:defun system::constant-warning (symbol form) ; ABI
    (unless custom:*suppress-check-redefinition*
      (warn (TEXT "~S redefines the constant ~S. Its old value was ~S.")
            form symbol (symbol-value symbol))))
  
  (setf clos::*warn-if-gf-already-called* nil))