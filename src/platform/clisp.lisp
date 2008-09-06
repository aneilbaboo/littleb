;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2003-2008 Aneil Mallavarapu

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.

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