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


;;; File: predicate
;;; Description: not predicates, and functions for defining predicates
;;;

;;; $Id: predicate.lisp,v 1.3 2008/09/06 00:23:09 amallavarapu Exp $
;;; $Log: predicate.lisp,v $
;;; Revision 1.3  2008/09/06 00:23:09  amallavarapu
;;; back to MIT license
;;;
;;; Revision 1.2  2008/09/02 14:58:11  amallavarapu
;;; Changed to GPL license
;;;
;;; Revision 1.1  2007/09/25 17:54:14  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.7  2007/09/25 16:22:45  am116
;;; *** empty log message ***
;;;
;;; Revision 1.6  2006/11/06 23:40:44  am116
;;; *** empty log message ***
;;;
;;; Revision 1.5  2006/11/02 23:53:26  am116
;;; Changed CLEAR/RESET system:
;;;
;;; RESET performs a minimal clear of the system:
;;;     * resets the database
;;;     * clears the *USER-LIBRARY* package (default=B-USER),
;;;     * reloads files which have been registered in the *reload-on-reset* list by RELOAD-ON-RESET
;;;     * loads the file (if any) associated with the package.
;;;
;;; CLEAR resets the database and empties all of the included packages.
;;;
;;; Revision 1.4  2005/11/09 16:51:00  am116
;;; *** empty log message ***
;;;
;;; Revision 1.3  2005/03/06 23:20:52  am116
;;; replaced neq, nequal... with the #! read macro which constructs a complement lambda
;;; replace unary-predicate and all the (n)eq...-predicates with the 'with' macro
;;;
;;; Revision 1.2  2004/11/01 21:14:07  Aneil
;;; *** empty log message ***
;;;
;;;
(in-package mallavar-utility)

(defun complement-macro-reader (stream dispchar subchar)
  (declare (ignorable dispchar subchar))
  `(lambda (&rest values) (not (apply #',(read stream nil nil t) values))))

(set-dispatch-macro-character #\# #\! #'complement-macro-reader)

(defun with (function val) 
  "Given a binary function (FN) and a value (VAL), returns a function which uses val as an implicit first argument.  E.g., (find-if (with #'eq 0) myseq) is the same as (find-if (lambda (o) (eq 0)) myseq)"
  (lambda (&rest values) (apply function val values)))
