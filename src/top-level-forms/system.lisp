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


;;; File: system
;;; Description: misc system functions accessible to the user
;;;

;;; $Id: system.lisp,v 1.4 2008/09/06 00:23:09 amallavarapu Exp $
;;;
(in-package b)


(defun retract (o)
  (if (conceptp o)
      (kb-retract o)
    (error "Attempt to retract ~A, which is not a concept object." o)))

(defmacro with-relevance (r &body body)
  `(let ((*relevance* ,r))
     ,@body))

(defun field-bound-p (obj fld)
  (ifit (fieldinfo obj fld)
      (let ((k (fieldinfo-kind it)))
        (ecase k
          (:property             (property-bound-p obj fld))
          ((:id :function)       t)))))

(defun property-bound-p (c p)
  "Returns T if property p is defined in concept c, or NIL.  An assertion is
triggered if P is not the name of a valid property in C."
  (concept-property-bound-p c p))
