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

;;; File: trace-filter
;;; Description: Support to add/remove predicates which determine whether a particular
;;;              object should be traced.

;;; $Id: trace-filter.lisp,v 1.1 2007/09/25 17:54:12 amallavarapu Exp $
;;; $Log: trace-filter.lisp,v $
;;; Revision 1.1  2007/09/25 17:54:12  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.5  2007/09/25 16:22:45  am116
;;; *** empty log message ***
;;;
;;; Revision 1.4  2006/11/06 23:40:44  am116
;;; *** empty log message ***
;;;
;;; Revision 1.3  2006/11/02 23:53:26  am116
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
;;; Revision 1.2  2006/10/23 17:11:23  am116
;;; *** empty log message ***
;;;
;;; Revision 1.1  2004/12/01 15:00:06  am116
;;; Preliminary code - not yet working
;;;

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +class-filter+ (make-symbol "CLASS-FILTER"))
(defconstant +property-filter+ (make-symbol "PROP-FILTER"))
(defconstant 

(defconstant +trace-filters+ (make-hash-table :test 'equal))

(defun class-filter-name (class)
  (list +class-filter+ class))

(defun class-filter (class)
  (trace-filter (class-filter-name class)))

(defun (setf class-filter) (val class)
  (setf (trace-filter (class-filter-name class)) 
          (ecase val
            (t   (lambda (o) (typep o class)))
            (nil nil))))

(defun trace-filter (name)
  (gethash name +trace-filters+))

(defun (setf trace-filter) (fn name)
  (if fn
      (setf (gethash name +trace-filters+) fn)
    (remhash name +trace-filters)))

)
