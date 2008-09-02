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

;;; File: error-info
;;; Description: utilities intended for providing and retrieving teleological
;;;              information accumulated as forms are executed.

;;; $Id: context-info.lisp,v 1.2 2008/09/02 14:58:11 amallavarapu Exp $
;;; $Log: context-info.lisp,v $
;;; Revision 1.2  2008/09/02 14:58:11  amallavarapu
;;; Changed to GPL license
;;;
;;; Revision 1.1  2007/09/25 17:54:14  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.3  2007/09/25 16:22:45  am116
;;; *** empty log message ***
;;;
;;; Revision 1.2  2004/11/24 00:47:29  am116
;;; Compiles OK.
;;; Added meta-reaction
;;;
;;; Revision 1.1  2004/11/08 12:07:53  CVS
;;; *** empty log message ***
;;;
;;;

(in-package mallavar-utility)


(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar      *context-info* ())

(defmacro with-context-info ((cstring &rest args) &body body)
  "Provides some data which can be used later to create a message that reflects
the teleological conext in which processing occurs.  Cstring & args are treated 
as a format string and arguments.  They should describe what the code in BODY is trying
to achieve.  End punctuation should be ommitted."
  (check-type cstring string)
  `(let ((*context-info* (cons ,cstring (cons (list ,@args) *context-info*))))
     ,@body))

(defun format-context-info (stream &key (separator nil) (prefix nil) (suffix nil) (reverse nil))
  (let ((context-info   (if reverse *context-info* (reverse *context-info*))))
    (when context-info
      (if prefix (princ prefix stream))    
      (let ((formatstr (format nil "~A~A~A" "~?~{" separator "~?~}")))
        (format stream formatstr (first context-info) (second context-info) (cddr context-info)))
      (if suffix (princ suffix stream)))))    

(defun error-with-context (options &optional estring &rest args)
  (let ((no-options (stringp options)))
    (let ((options    (if no-options '(:separator "-->" :prefix " while")))
          (args       (if no-options (cons estring args) args))
          (estring    (if no-options options estring)))

    (error (format nil "~?~A." estring args (apply #'format-context-info nil options))))))
)
