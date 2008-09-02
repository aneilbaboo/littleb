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

;;; File: math-concepts
;;; Description: Defines base classes, which affects how instances are treated
;;;              within the mathematics operator.
;;;              Instances of objects not derived from these classes
;;;              will be treated as variables under polynomial arithmetic.
;;;
;;;              However, objects of this class will not be wrapped in polynomials,
;;;              and may be processed by any methods which act on them.
;;;              I.e., *op, +op, ^op methods should be defined for subclasses of 
;;;              arithmetic-value and any combinations.
;;;              

;;; $Id: math-concept.lisp,v 1.8 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Log $

(in-package b/math)

(include-documentation
  :description ""
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

(defcon math-concept (:abstract) ())

(defield math-concept.terms () object)

(defield number.terms ()
  (if (zerop object) nil object))

(defgeneric math-expression-to-list (self)
  (:method (self) self))
;;;
;;; LET method
;;;  
(defun math-let-substituter (bindings)
  (lambda (var)
    (getf bindings var var)))

(defield math-concept.let (&rest bindings)
   (map-vars object (math-let-substituter bindings) nil))

(defmethod fld ((o null) (field (eql :vars)) &rest args)
  (declare (ignore o field args)) 
  nil)

(defield math-concept.vars () nil)

(defield math-concept.eval-vars ()
  (map-vars object (lambda (o) (if (reference-var-p o) o.value o)) nil))

(defield math-concept.expand (n) 
  (declare (ignorable n)) 
  object)
  
(defcon numeric-concept (:abstract math-concept)
  ())
       

(defmethod fld ((o number) (field (eql :vars)) &rest args)
  (declare (ignorable o field args))
  nil)

(declaim (inline simplify-number))
(defun simplify-number (x) (if (and (rationalp x) (not (= 1 (denominator x))))
                               (coerce x 'float) x))
