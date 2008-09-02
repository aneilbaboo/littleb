;;;; This file is part of little b.

;;;; Copyright (c) 2005-8 Aneil Mallavarapu

;;;; Little b is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Little b is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with little b.  If not, see <http://www.gnu.org/licenses/>.
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