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

;;; File: dimensional-methods
;;; Description: 

;;; $Id: dimensional-methods.lisp,v 1.4 2008/09/02 14:58:10 amallavarapu Exp $
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
(defmethod magnitude-of ((n number) &optional u) (declare (ignore u)) n)
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

(def-multitype-method get-math-expression-vars ((o number numeric-concept dimension unit))
   (declare (ignorable o)) nil)

(defmethod get-math-expression-vars (o) (list o))

(defmethod get-math-expression-vars ((o sum-expression))
  (delete-duplicates (mapcan (lambda (se) (get-math-expression-vars (sum-element-var se)))
                             o.variable)))

(defmethod get-math-expression-vars ((o product-expression))
  (delete-duplicates (mapcan (lambda (pe) 
                               (nconc (get-math-expression-vars (product-element-var pe))
                                      (get-math-expression-vars (product-element-power pe))))
                             o.variable)))
