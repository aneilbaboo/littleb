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

;;; File: product-expression
;;; Description: defines product-expression

(in-package #I@FOLDER)

(include (@LIBRARY/util
          @FOLDER/DTYPE
          @FOLDER/math-expression))

(defcon product-expression (:notrace :non-matchable math-expression)
  (variable)
  (unless (listp variable)
    (b-error "Invalid argument to sum-expression - ~S - expecting a list." variable)))


(defun product-expression (factors)
  (flet ((single-simple-factor-p (elts)
           (and (eq (length elts) 1)
                (equalp (product-element-power (first elts)) 1))))
    (let* ((coefficient 1)
           (new-factors (loop for pe in factors 
                              for var = (product-element-var pe)
                              for power = (product-element-power pe)
                              do (assert (not (product-expression-p var)))
                              if (and (numericp var) (numberp power))
                              do (setf coefficient (*op coefficient (^op var power)))
                              else unless (or (eq power 0) (eq power 0.0))
                              collect pe)))
      (cond
       ((null new-factors)                     coefficient)
       ((single-simple-factor-p new-factors)   (multiplicity-sum-expression (product-element-var (first new-factors))
                                                                      coefficient))
       ((equalp coefficient 1)                 [product-expression new-factors])
       (t                                      (multiplicity-sum-expression
                                                [product-expression new-factors] coefficient))))))


(defmethod math-expression-to-list ((self product-expression))
  (flet ((pe-to-list (pe)
           (let ((var (product-element-var pe))
                 (power (product-element-power pe)))
             (cond
              ((eq power 1) var)
              (t            `(^ ,(math-expression-to-list var)
                                ,(math-expression-to-list power)))))))
    (let ((elts (mapcar #'pe-to-list self.variable)))
      (cond
       ((> (length elts) 1) `(* ,@elts))
       (t                   (first elts))))))
                         

(defield product-expression.vars ()
  (get-math-expression-vars object))

(defield product-expression.dimension ()
  (dimension-of object))

(predefine (:const null-dimension)
(defun product-element (var &optional (power 1)) 
  (b-assert (and power (no-dimension-p (dimension-of power))) ()
    "{~S ^ ~S} is invalid.  Exponents must be null-dimension values, but ~S was received."
    var power (dimension-of power))
  (cons var power)))
(defun product-element-var (o) (car o))
(defun product-element-power (o) (cdr o))

(defun invert-product-elements (pes)
  (mapcar (lambda (pe) (product-element (product-element-var pe)
                                        (*op (product-element-power pe) -1)))
          pes))

(defun product-expression-denominator-only-p (p)
  (assert (product-expression-p p))
  (unless (some (compose #'positive-p #'product-element-power) 
                (|PRODUCT-EXPRESSION.VARIABLE| p)) t))    

(defield product-expression.expand-1 ()
  (loop for v1 = (first .variable) then (first factors)
        for factors = (rest v1)))

(defield product-expression.map (fn &rest operator-substitutions)
  (map-vars (the product-expression object) fn operator-substitutions))

(defmethod dimension-of ((pe product-expression))
  (loop for elt in (|PRODUCT-EXPRESSION.VARIABLE| pe)
        for v = (product-element-var elt)
        for p = (product-element-power elt)
        for dim = (^op (dimension-of v) p) then (*op (^op (dimension-of v) p) dim)
        finally (return dim)))


(defgeneric map-vars (o fn &optional operator-substitutions))

(def-multitype-method map-vars ((o number numeric-concept) fn  &optional operator-substitutions)
  (declare(ignorable fn operator-substitutions))
  o)

(defmethod map-vars (o fn &optional operator-substitutions) 
  (declare (ignorable operator-substitutions))
  (funcall fn o))

(defmethod map-vars ((o product-expression) fn &optional operator-substitutions)
  (cond 
   (operator-substitutions
    (loop with val
          for pe in (|PRODUCT-EXPRESSION.VARIABLE| o)
          for var = (map-vars (product-element-var pe) fn operator-substitutions)
          for power = (map-vars (product-element-power pe) fn operator-substitutions)
          for subst-factor = (call-op-subst '^ operator-substitutions
                                            var power)
          do (setf val (if val (call-op-subst '* operator-substitutions
                                              val subst-factor) subst-factor))
          finally (return val)))
    (t (loop with val
          for pe in (|PRODUCT-EXPRESSION.VARIABLE| o)
          for var = (map-vars (product-element-var pe) fn operator-substitutions)
          for power = (map-vars (product-element-power pe) fn operator-substitutions)
          for subst-factor = (^op var power)
          do (setf val (if val (*op val subst-factor) subst-factor))
          finally (return val)))))
 
(defmethod map-subst ((o product-expression) fn)
  [product-expression (mapcar (lambda (pe)
                                (product-element
                                 (map-subst (product-element-var pe) fn)
                                 (map-subst (product-element-power pe) fn)))
                              (|PRODUCT-EXPRESSION.VARIABLE| o))])
        
