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

;;; File: type
;;; Description: for determining the type of objects based on the class/field 

;;; $Id: type.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;
;; Type determination functions - 
;;      Useful for building rule patterns, amongst other things
;;

;;;; (defmethod get-field-type ((cclass concept-class) field)
;;;;   (fieldinfo-type (class-fieldinfo cclass field)))

;;;; (defmethod get-field-type ((iclass interface-class) field)
;;;;   (iclass-field-type iclass field))
;;;;   
;;;; (defmethod get-field-type ((c concept) field)
;;;;   (get-field-type (class-of c) field))

;;;; (defmethod get-field-type ((s symbol) field)
;;;;   (get-field-type (global-value s) field))

;;;; (defmethod get-field-type ((f cons) field)
;;;;   (cond
;;;;    ((global-const-fld-form-p f)
;;;;     (get-field-type (fld-form-object f) (fld-form-field f)))

;;;;    ((fld-form-p f)
;;;;     (let ((o   (fld-form-object f))
;;;;           (fld (fld-form-field f)))
;;;;       (cond
;;;;        ((the-expr-p o)
;;;;         (get-field-type (find-class (the-expr-type o)) fld))
;;;;        ((fld-form-p o)
;;;;         (get-field-type (find-class 
;;;;                               (get-field-type (fld-form-object o)
;;;;                                                    (fld-form-field fld)))
;;;;                              fld))
;;;;        (t (error "Cannot compute field type of ~A" (print-b-expr `(FLD ,f ,field)))))))))

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
)
     
     
