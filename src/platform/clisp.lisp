(in-package :cl-user)

(port:allowing-redefinitions ()
  (defun system::constant-warning (symbol form) ; ABI
    (unless custom:*suppress-check-redefinition*
      (warn "~S redefines the constant ~S. Its old value was ~S."
            form symbol (symbol-value symbol)))))

(in-package :clos)
(port:allowing-redefinitions ()
  (setf *warn-if-gf-already-called* nil))

(import 'cl-user::quit 'b)
(export 'cl-user::quit 'b)