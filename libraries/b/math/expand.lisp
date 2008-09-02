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

;;; File: expand.lisp
;;; Description: expands mathematical expressions

(in-package b/math)

(include (@library/util
          @folder/math-expression 
          @folder/sum-expression
          @folder/product-expression))

(defparameter *expansion-steps* t)

(defun multiply-through (s &rest factors)
  (declare (type sum-expression x))
  (loop for total = (apply #'s* (|SUM-EXPRESSION.NUMERIC| s) factors)
               then (+op total (apply #'s* var coef factors))
        for se in (|SUM-EXPRESSION.VARIABLE| s)
        for var = (sum-element-var se)
        for coef = (sum-element-coef se)
        finally (return total)))

(defun s*expand (&rest args)
  (cond
   ((expand-p) (if (> (length args) 1) 
                   (reduce #'*op-expand args)
                 (first args)))

   (t          (apply #'s* args))))

(defun s^expand (&rest args)
  (cond
   ((expand-p)  (reduce (lambda (pow obj)
                          (^op-expand obj pow))
                        (reverse args)))

   (t           (apply #'s^ args))))      

(def-commutative-operator-method *op-expand ((s sum-expression) x)
  (multiply-through s x))


(defmethod *op-expand ((s1 sum-expression) (s2 sum-expression))
  (loop for total = (multiply-through s1 (|SUM-EXPRESSION.NUMERIC| s2))
        then (+op total (multiply-through s1 coef var))
        for se in (|SUM-EXPRESSION.VARIABLE| s2)
        for var = (sum-element-var se)
        for coef = (sum-element-coef se)
        finally (return total)))

(defmethod *op-expand (o1 o2)
  (*op o1 o2))

(defmethod ^op-expand ((s sum-expression) (n integer))
  (let+ (((p pos) (positive-value n)))
    (loop for i from 1 to p
          for total = s
          then (*op-expand total s)
          finally (return (if pos total
                            (^op total -1))))))
        
(defmethod ^op-expand (o1 o2)
  (^op o1 o2))

(defmacro with-next-expansion-step (&body body)
  `(let ((*expansion-steps* (if (eq *expansion-steps* t) t
                              (1- *expansion-steps*))))
     ,@body))

(defun expand-p () (if (or (eq *expansion-steps* t)
                           (> *expansion-steps* 0)) t))

(defmethod expand :around (o &optional (*expansion-steps* *expansion-steps*))
  (if (expand-p) 
      (with-next-expansion-step (call-next-method))
    o))

(defmethod expand (o &optional (*expansion-steps* *expansion-steps*)) o)

(defmethod expand ((s sum-expression) &optional (*expansion-steps* *expansion-steps*))
  (cond
   ((unit-sum-expression-p s) 
    (expand (first (sum-element-var (|SUM-EXPRESSION.VARIABLE| s)))))
   (t
    (loop for total = (|SUM-EXPRESSION.NUMERIC| s)
          then (+op total (*op-expand var coef))
          for se in (|SUM-EXPRESSION.VARIABLE| s)
          for coef = (sum-element-coef se)
          for var =  (with-next-expansion-step (expand (sum-element-var se)))
          finally (return total)))))
        
(defmethod expand ((p product-expression) &optional (*expansion-steps* *expansion-steps*))
  (loop for pe in (|PRODUCT-EXPRESSION.VARIABLE| p)
        for var = (with-next-expansion-step (expand (product-element-var pe)))
        for power = (with-next-expansion-step (expand (product-element-power pe)))
        for total = (^op-expand var power)
               then (*op-expand total (^op-expand var power))
        finally (return total)))

(defield math-expression.expand (&optional (steps *expansion-steps*))
  (let ((*expansion-steps* steps)) (expand object)))

(defmethod fld ((o number) (field (eql :expand)) &rest args)
  (declare (ignorable field args))
  o)
