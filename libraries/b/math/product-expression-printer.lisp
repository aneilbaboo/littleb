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


;;; File: sum-expression-printer
;;; Description: 

(in-package b/math)

(include-documentation
  :description "Basic functionality for printing product-expressions"
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

;;;;
;;; PRODUCT-EXPRESSION PRINTER:
;;;
(defmethod print-math-expression ((p product-expression)
                                  &optional (stream *standard-output*) (outer-op t))
  (let+ (((num den)  (loop for pe in p.variable
                           for pos-power = (positive-value (product-element-power pe))
                           for num? = (positive-p (product-element-power pe))
                           if num?
                           collect pe into numerator
                           else 
                           collect (product-element (product-element-var pe) 
                                                    pos-power)
                           into denominator
                           finally (return (values numerator denominator))))
         (prod-op-prec  (cond ;; compute the active operator given the 
                         ((> (+ (length num) (length den)) 1) '*) ; multiple factors => multiplication
                         ((or (unit-product-element-list-p num) ; no factors or powers
                              (unit-product-element-list-p den)) t)
                         (t                                    '^))) ; exponentiation only
         (outer-braces? (braces-required-p outer-op prod-op-prec))
         (new-outer-op  (if outer-braces? '* (lower-operator '* outer-op))))
    (pprint-math-block (stream outer-braces?)
      (cond
       (num  (print-factors num stream new-outer-op)
             (when den
               (math-newline)
               (print-math '/ stream)
               (print-math-expression (product-expression den) stream (1- (operator-precedence '/)))))
       (t    (print-factors (invert-product-elements den) stream
                            new-outer-op))))))

(defun unit-product-element-list-p (pes)
  (and (= (length pes) 1)
       (let ((power (product-element-power (first pes))))
         (and (numberp power)
              (= 1 power)))))

(defun all-denominator-product-elements-p (pes)
  (notany (compose #'positive-p #'product-element-power) pes))

(defun compute-precedence-from-pe-list (p)
  (cond 
   ((eq (length p) 1) 
    (if (equalp 1 (product-element-power (first p))) t '^))
   (t '*)))

(defun print-factors (pe-list stream outer-op)
  (let ((outer-braces? (braces-required-p outer-op '*)))
    (pprint-math-block (stream outer-braces?)
      (locally (declare (ignorable first-factor-p))
        (loop for pe in pe-list
              for var = (product-element-var pe)
              for power = (product-element-power pe)
              for active-op = (if outer-braces? nil outer-op)
              then '*
              for first-factor-p = nil then (progn (math-newline) (print-math '* stream))
              if (not (eql power 1))
              do (print-math var stream '^)
              (print-math '^ stream)
              (print-math power stream '^)
              else
              do (print-math var stream active-op))))))
  
(defmethod print-object ((prod product-expression) stream)
  (with-print-context prod
    (if *debug-printing*
        (print-concept prod stream)
      (print-math prod stream t))))
