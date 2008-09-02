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

;;; File: sum-expression
;;; Description: defines sum-expression

(in-package #I@FOLDER)


(include (@LIBRARY/util
          @FOLDER/DTYPE
          @FOLDER/math-expression))

(defparameter *print-default-operator* t)

(defcon sum-expression (:notrace :non-matchable math-expression)
  (variable numeric)
  (unless (listp variable)
    (b-error "Invalid argument to sum-expression - ~S - expecting a list." variable)))


(defield sum-expression.terms ()
  (let ((sterms (mapcar (lambda (se)
                          (s* (sum-element-coef se) * (sum-element-var se)))
                        .variable)))
    (if (zero-numeric-p .numeric) sterms
      (list* .numeric sterms))))

(defmethod math-expression-to-list ((self sum-expression))
  (flet ((se-to-prefix (se)
           (let ((var (math-expression-to-list (sum-element-var se)))
                 (coef (math-expression-to-list (sum-element-coef se))))
             (cond
              ((eq coef 1) var)
              (t           `(* ,coef ,var))))))
    (let ((elts `(,@(mapcar #'se-to-prefix self.variable)
                  ,@(unless (zerop self.numeric)
                      (list self.numeric)))))
      (cond
       ((> (length elts) 1) `(+ ,@elts))
       (t                   (first elts))))))
      

;;;
;;; constructors
;;; 
(defun sum-expression  (variable &optional numeric)
  (let* ((numeric (or numeric
                      (quantity 0 (dimension-of
                                   (sum-element-var (first variable))))))
         (variable (remove-if (lambda (o) (zero-numeric-p (sum-element-coef o))) variable))
         (len      (length variable)))

    ;;(sum-expression-dimension-sanity-test variable numeric)

    (cond 
      ((eql len 0)                      numeric)
      ((and (eql len 1)
            (zero-numeric-p numeric))   (let* ((se (first variable))
                                               (v  (sum-element-var se))
                                               (c  (sum-element-coef se)))
                                          (cond
                                           ((or (unit-p v) (dimension-p v))          (*op v c)) ; if v is a unit or dimension, 
                                           ((and (product-expression-p v)
                                                 (eql c 1))                          v)
                                           (t                                        [sum-expression variable numeric]))))
      (t                               [sum-expression variable numeric]))))

;;;
;;; STRUCTURE & DIMENSION SANITY TEST - remove in final code
;;;
(defun sum-expression-dimension-sanity-test (variable numeric)
  (let ((d        (dimension-of numeric)))
    (dolist (se variable)
      (if (unit-sum-expression-p (sum-element-var se))
          (error "contains unit sum expr"))
      (let ((d2 (*op (dimension-of (sum-element-var se))
                     (dimension-of (sum-element-coef se)))))
        (unless (eq d d2)
          (error "inconsistent sum expression dimensions: ~%~S <> ~S" d d2))))))


(defield sum-expression.dimension ()
  (dimension-of object))


(defield sum-expression.vars ()
  (get-math-expression-vars object))

(defield sum-expression.as-list ()
  "Converts a sum-expression (with positive integer coefficients only) into a list containing the elements:
   {:a + 2 :b}.as-list => (:A :B :B)
   {:a + 2.5 :b}.as-list => error"
  (loop for se in .variable
        for c = (sum-element-coef se)
        for v = (sum-element-var se)
        unless (and (integerp c)
                    (plusp c))
        do (b-error "Cannot convert ~S to a list - ~S is not a positive integer coefficient"
                    object c)
        append (make-list c :initial-element v)))

;;;
;;; MULTIPLICITY-SUM-EXPRESSION: a sum expression of one variable and coefficient
;;;
(defmacro with-multiplicity-sum-expression (sum-expr (var coef) &body body)
  (let ((first-elt (gensym "FIRST-ELT")))
    `(let* ((,first-elt (first (fld ,sum-expr :variable)))
            (,var       (sum-element-var ,first-elt))
            (,coef      (sum-element-coef ,first-elt)))
       ,@body)))

(defun multiplicity-sum-expression (o &optional (coef 1))
  (typecase o
    (sum-expression (if (eq coef 1) o (sum-expression (list (sum-element o coef))
                                                     (quantity 0 (*op (dimension-of o) (dimension-of coef))))))
    (otherwise      (sum-expression (list (sum-element o coef))
                                    (quantity 0 (*op (dimension-of o) (dimension-of coef)))))))

;+-----------------------------
;+ dirty hack - avert eyes
;+-----------------------------
(define-function make-math-expression (o) 
  "This function is provided to plug b/MATH into the math-reader's AS-MATH-ARG function"
 (multiplicity-sum-expression o))

;;;
;;; MULTIPLICITY-SUM-EXPRESSION: a sum expression where a single element 
;;;                              is multiplied by a coefficient
;;;
(defun multiplicity-sum-expression-p (o)
  (and (sum-expression-p o)
       (let ((var (the sum-expression o).variable)
             (num (the sum-expression o).numeric))
         (and (eq (length var) 1)
              (zero-numeric-p num)))))
         
;;;
;;; UNIT-SUM-EXPRESSION: a sum expression of one variable where coefficient = 1
;;;
(defun unit-sum-expression-p (o)
  (and (sum-expression-p o)
       (let ((var (the sum-expression o).variable)
             (num (the sum-expression o).numeric))
         (and (eq (length var) 1)
              (zero-numeric-p num)
              (equalp (sum-element-coef (first var)) 1)))))


(defun sum-element (var &optional (coef 1)) 
  (assert (numericp coef))
  (cons var coef))
(defun sum-element-var (o) (car o))
(defun sum-element-coef (o) (cdr o))


;;;
;;; MAP methods
;;;
(defield sum-expression.map (fn &rest operator-substitutions) 
  (map-vars (the sum-expression object) fn operator-substitutions)) 

(defield sum-expression.map-terms (&optional (fn (lambda (var coef) (cons var coef))))
  "Takes a function FN of two arguments (var coef), where var is the
variable part and coef is the numeric coefficient multiplier of var,
and returns a list of values returned by FN.  If the numeric part of
sum expression is non-zero, then FN is called with (NIL N), where N
is the numeric component."
;  (let ((num .numeric))
  (let ((num (|SUM-EXPRESSION.NUMERIC| object)))
    (mapcar (lambda (se)
              (funcall fn (sum-element-var se) (sum-element-coef se)))
            (if (zero-numeric-p num) (|SUM-EXPRESSION.VARIABLE| object)
              (cons (sum-element nil num) (|SUM-EXPRESSION.VARIABLE| object))))))

(defmethod dimension-of ((se sum-expression))
  (dimension-of (|SUM-EXPRESSION.NUMERIC| se)))

(defmethod map-vars ((o sum-expression) fn  &optional operator-substitutions)
  (cond 
   (operator-substitutions
    (loop with numeric = (|SUM-EXPRESSION.NUMERIC| o)
          for se in (|SUM-EXPRESSION.VARIABLE| o)
          for var = (map-vars (sum-element-var se) fn operator-substitutions)
          for coef = (map-vars (sum-element-coef se) fn operator-substitutions)
          for subst-term = (call-op-subst '* operator-substitutions 
                                          var coef)
          for val = (if (zero-numeric-p numeric) subst-term
                      (call-op-subst '+ operator-substitutions
                                     numeric  subst-term))
          then (call-op-subst '+ operator-substitutions
                              `                 val subst-term)
          finally (return val)))
   (t
    (loop with numeric = (|SUM-EXPRESSION.NUMERIC| o)
          for se in (|SUM-EXPRESSION.VARIABLE| o)
          for var = (map-vars (sum-element-var se) fn nil)
          for coef = (map-vars (sum-element-coef se) fn nil)
          for subst-term = (*op var coef)
          for val = (if (zero-numeric-p numeric) subst-term
                      (+op numeric subst-term))
               then (+op val subst-term)
          finally (return val)))))

(defmethod map-subst ((o sum-expression) fn)
  [sum-expression (mapcar (lambda (se)
                            (sum-element 
                             (map-subst (sum-element-var se) fn)
                             (map-subst (sum-element-coef se) fn)))
                          (|SUM-EXPRESSION.VARIABLE| o))
                  (|SUM-EXPRESSION.NUMERIC| o)])
                                  

(def-multitype-method map-subst ((o numeric-concept number) fn) (declare (ignorable fn)) o)
(defmethod map-subst (o fn) (funcall fn o))
  
(defield math-concept.map-substitution (fn) (map-subst object fn))
(defmethod fld ((o number) (field (eql :map-substitution)) &rest args)
  (declare (ignorable o field args))
  o)

(defield math-concept.sub (subs &optional (test #'eql)) 
  (map-subst object (lambda (o)
                      (or (second (assoc o subs :test test)) o))))
(defmethod fld ((o number) (field (eql :sub)) &rest args)
  (declare (ignore field args))
  o)
  
(defmethod fld ((o list) (field (eql :vars)) &rest args)
  (declare (ignore field args))
  o)

(defmethod fld (o (field (eql :vars)) &rest args)
  (declare (ignore field args))
  (list o))

