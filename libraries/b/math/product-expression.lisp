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
        
