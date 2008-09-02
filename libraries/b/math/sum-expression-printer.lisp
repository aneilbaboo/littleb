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

;;; File: sum-expression-printer
;;; Description: 

(in-package b/math)

(include-documentation
  :description "Basic functionality for printing sum-expressions"
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

;;;;
;;;;
;;;; SUM-EXPRESSION PRINTER:
;;;;
;;;;
(defmethod print-math-expression ((s sum-expression) &optional (stream *standard-output*) (outer-op t))
  (let* ((outer-braces? (braces-required-p outer-op (cond
                                                     ((unit-sum-expression-p s) t)
                                                     ((multiplicity-sum-expression-p s) '*)
                                                     (t '+))))
         (sorted-variable (sort (copy-list s.variable) 
                                (lambda (outer inner) (and (positive-p outer) (not (positive-p inner))))
                                :key #'sum-element-coef))
         (print-num-first (and (not (positive-p (sum-element-coef (first sorted-variable))))
                               (positive-p s.numeric))))
    (pprint-math-block (stream outer-braces?)

      (when (and print-num-first (not (zero-numeric-p s.numeric)))
        (print-math s.numeric stream))
 
      (loop for new-outer-op = (if outer-braces? '+ outer-op) then '+
            for first-term-p = t then nil
            for se in sorted-variable
            for var = (sum-element-var se)
            for coef = (sum-element-coef se)
            do (let+ (((pos-coef pos?) (positive-value coef)))
                 (unless (and first-term-p pos?)
                   (math-newline)
                   (print-math (if pos? '+ '-) stream))
                   
                 (print-sum-expression-term pos-coef var stream new-outer-op)))

      (unless (or print-num-first 
                  (zero-numeric-p s.numeric))
        (let+ (((pos-val pos?) (positive-value s.numeric)))
          (print-math (if pos? '+ '-) stream)
          (print-math pos-val stream '+))))))

(defun print-sum-expression-term (coef var stream outer-op)
  (cond
   ;; product expression with only negative exponents:
   ;;  PRINT: POS-COEF / INVERSE-PROD-EXPR
   ((and (product-expression-p var)
         (product-expression-denominator-only-p var))
    (print-math coef stream outer-op)
    (print-math '/ stream)
    (print-math (^op var -1) stream 
                (1- (operator-precedence '/))))
                  
   ;; IF COEF = 1, ignore it
   ((equalp coef 1)
    (print-math var stream outer-op))

   ;; default printing
   (t (print-math coef stream outer-op)
      (let ((*print-default-operator* nil))
        (print-math '* stream))
      (print-math var stream '*))))

(defmethod print-object ((s sum-expression) stream)
  (with-print-context s
    (if *debug-printing* (print-concept s stream)
      (print-math s stream))))
