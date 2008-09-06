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


;;; File: error-info
;;; Description: utilities intended for providing and retrieving teleological
;;;              information accumulated as forms are executed.

;;; $Id: context-info.lisp,v 1.3 2008/09/06 00:23:09 amallavarapu Exp $
;;; $Log: context-info.lisp,v $
;;; Revision 1.3  2008/09/06 00:23:09  amallavarapu
;;; back to MIT license
;;;
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
