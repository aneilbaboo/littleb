(in-package :cl-user)

(port:allowing-redefinitions ()
  (defun system::constant-warning (symbol form) ; ABI
    (unless custom:*suppress-check-redefinition*
      (warn "~S redefines the constant ~S. Its old value was ~S."
            form symbol (symbol-value symbol)))))

(in-package :clos)
(port:allowing-redefinitions ()
  (setf *warn-if-gf-already-called* nil ; pre-2.43 versions
        *enable-clos-warnings* nil))    ; 2.43 and beyond

;; a bug workaround - clisp defconstant calls constant-warning with wrong # of args:
#+:clisp
(ignore-errors 
  (let ((clisp-version (read-from-string (lisp-implementation-version))))
    (when (and (numberp clisp-version)
               (> clisp-version 2.44))
      (defun system::constant-warning (&rest args) ()))))