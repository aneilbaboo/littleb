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

;;; File: missing
;;; Description: defines ?, a constant which represents "missing"

;;; $Id: missing.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Log: missing.lisp,v $
;;; Revision 1.2  2008/09/02 14:58:10  amallavarapu
;;; Changed to GPL license
;;;
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
