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

;;; $Id: math-concept.lisp,v 1.11 2008/10/31 00:05:47 amallavarapu Exp $
;;; $Log $

(in-package b/math)

(include-documentation
  :description ""
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

(defcon math-concept (:abstract) ())

(defield math-concept.terms () object)

(defmethod fld ((object number) (field (eql :terms)) &rest args)
  (declare (ignore field args))
  (if (zerop object) nil object))

(defgeneric math-expression-to-list (self)
  (:method (self) self))
;;;
;;; LET method
;;;  
(defield number.let (&rest bindings)
  (declare (ignore bindings))
  object)
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
