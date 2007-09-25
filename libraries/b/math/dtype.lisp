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

;;; File: type
;;; Description: for determining the type of objects based on the class/field 

;;; $Id: dtype.lisp,v 1.1 2007/09/25 17:54:05 amallavarapu Exp $
;;;

(in-package #I@FOLDER)

(include @/math-concept :use)
;;;
;;;
;;; DTypes - are dimensional types - types which take a dimension as an argument
;;;          DTYPE specifiers must be checked with dtypep, not typep.
;;;
(defmacro defdtype (name &body body)
  `(progn 
     (setf (get ',name 'dimensional-type) t)
     (deftype ,name ()
       ,@body)))

(defun dtype-expr-p (e)
  (or (dimension-p e)      
      (and (consp e) (= 2 (length e))
           (get (first e) 'dimensional-type))))

(defun dtypep (obj type)
  "Extends functionality of typep for checking dimension for numeric and value types."
  (cond
   ((dimension-p type)      (eq (dimension-of obj) type))
   ((dtype-expr-p type)  (let ((dim (dtype-expr-dimension type)))
                           (ignore-errors 
                             (and (typep obj (first type))
                                  (eq (dimension-of obj) dim)))))
   (t                    (xtypep obj type))))


(defun dtype-expr-dimension (dtype-expr)
  "Returns: DIMENSION.  Dimension is a dimension object (or NIL)"
  (typecase dtype-expr
    (dimension  dtype-expr)
    (cons       (dtype-expr-dimension (second dtype-expr)))
    (null       nil)
    (symbol     (let ((dim (global-value dtype-expr t)))
                  (assert (dimension-p dim) ()
                    "~S is not a dimension" dtype-expr)
                  dim))
    (t          (error "~S is not a valid dimensional type expression" dtype-expr))))

(defdtype numeric 
  `(or numeric-concept number))

(defun numericp (o)
  (or (numeric-concept-p o) (numberp o)))

(defdtype value 
  `(or numeric var))

(defun valuep (o)
  (or (numericp o) (var-p o)))
