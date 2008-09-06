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
