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

;;; File: expand.lisp
;;; Description: expands mathematical expressions

(in-package b/math)

(include (@library/util
          @folder/math-expression 
          @folder/sum-expression
          @folder/product-expression))

(defparameter *expansion-steps* t)

(defun multiply-through (s &rest factors)
  (declare (sum-expression x))
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
