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

;;; File: dimensional-methods
;;; Description: 

;;; $Id: dimensional-methods.lisp,v 1.1 2007/09/25 17:54:05 amallavarapu Exp $
;;;
(in-package b/math)

;; Generic Methods for dealing with dimensional objects:

;; dimension-of
(defgeneric dimension-of (o))

;; MAGNITUDE - generic method
(defgeneric magnitude-of (q &optional u))
(defmethod magnitude-of :around (q &optional u)
  (if (and (dimension-p u)
           (#!eq (dimension-of u) (dimension-of q)))
      (error "Attempt to determine magnitude of ~S in units of a different dimension (~S)."
             q u))
  (call-next-method))
(defmethod magnitude-of ((n number) &optional u)  n)
(defmethod magnitude-of ((u unit) &optional units)
  (let ((u-in-base (convert-to-unit 1 u t)))
    (if units
        (convert-to-unit u-in-base u)
      u-in-base)))


(defmethod negativep ((n number))
  (minusp n))

(defmethod negativep ((c complex))
  (negativep (realpart c)))

;;;;
;;;; for want of a better place to put these methods:
;;;;

(def-multitype-method get-math-expression-vars ((o number numeric-concept dimension unit))  nil)

(defmethod get-math-expression-vars (o) (list o))

(defmethod get-math-expression-vars ((o sum-expression))
  (delete-duplicates (mapcan (lambda (se) (get-math-expression-vars (sum-element-var se)))
                             o.variable)))

(defmethod get-math-expression-vars ((o product-expression))
  (delete-duplicates (mapcan (lambda (pe) 
                               (nconc (get-math-expression-vars (product-element-var pe))
                                      (get-math-expression-vars (product-element-power pe))))
                             o.variable)))
