;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2007 Aneil Mallavarapu

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

;;; File: missing
;;; Description: defines ?, a constant which represents "missing"

;;; $Id: missing.lisp,v 1.1 2007/09/25 17:54:12 amallavarapu Exp $
;;; $Log: missing.lisp,v $
;;; Revision 1.1  2007/09/25 17:54:12  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.9  2007/09/25 16:22:45  am116
;;; *** empty log message ***
;;;
;;; Revision 1.8  2006/11/06 23:40:44  am116
;;; *** empty log message ***
;;;
;;; Revision 1.7  2006/11/02 23:53:26  am116
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
;;; Revision 1.6  2005/03/16 22:55:25  am116
;;; cleaned up missing functions
;;;
;;; Revision 1.5  2004/11/05 00:02:46  Aneil
;;; *** empty log message ***
;;;
;;; Revision 1.4  2004/11/01 21:14:04  Aneil
;;; *** empty log message ***
;;;
;;;

(in-package b)
;;; ? = Missing.  Indicates that a value
(defconstant ? '?)

;;;; (defmethod get-field ((o (eql ?)) field &optional funcall &rest args)
;;;;   ?)

(declaim (inline missingp))
(defun missingp (o)
  (eq o ?))

;;;; (defmacro if? (test then &optional (else ?))
;;;;   "Like IF, but ? substitutes for NIL."
;;;;   `(if (missingp ,test) ,then ,else))

;;;; (defmacro or? (&rest forms)
;;;;   "Like OR, but returns ? for false."
;;;;   (let ((or-result (gensym "RET")))
;;;;     `(let ((,or-result (or ,@forms)))
;;;;        (

;;;; (defmacro and? (&rest forms)
;;;;   "Like AND, but returns ? for false."
;;;;   `(or (and ,@forms) ?))

;;;; (defmacro not? (object)
;;;;   "Like NOT, but returns ? for false."
;;;;   `(or (not ,object) ?))
