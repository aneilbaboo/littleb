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

;;; File: range
;;; Description: defines a range of values
;;;  not working - don't understand the semantics for exponentiation 

;;; $Id: range.lisp,v 1.1 2007/09/25 17:54:05 amallavarapu Exp $

(in-package #I@FILE)

(include @FOLDER :use)
(defcon range ()
  (mean spread)
  (assert (and (realp spread)
               (realp mean)
               (plusp spread)) ()
      "Invalid range.  Expecting [range mean spread], where mean and spread are ~
reals, and spread is positive.")
  (fields :mean mean :spread spread))

(defield range.min ()
  (- mean spread))

(defield range.max ()
  (+ mean spread))

(defmethod *op ((r range) (n number))
  (range (* r.mean n) (abs (* r.spread n))))

(defmethod +op ((r range) (n number))
  (range (+ r.mean n) r.spread))

(defmethod *op ((r1 range) (r2 range))
  (range (* r1.mean r2.mean) (abs (* r1.spread r2.spread))))

(defmethod +op ((r1 range) (r2 range))
  #[range (+ r1.mean r2.mean) (+ r1.spread r2.spread)])

(defmethod reduce-op ((r range)) +irreducible-value+)

(defmethod *op ((r range) (u unit))
  (quantity (range (convert-to-unit r.mean u t)
                   (convert-to-unit r.spread u t))
            u.dimension))

(defmethod convert-to-unit ((r range) u &optional inverse power)
  (range (convert-to-unit r.mean u inverse power)
         (convert-to-unit r.spread u inverse power)))

(defmethod magnitude-of ((r range) &optional u) r.mean)

(defmethod ^op ((r range) (p number))
  (b-ERROR "THIS METHOD IS NOT FINISHED!")
  (if (and (minusp r.min)
           (or (zerop r.man) (plusp r.max)))
    
      (range (careful-expt r.mean p) (careful-expt r.spread (abs p)))))

(defun range (mean spread)
  (if (zerop spread) mean
    #[range mean spread]))

(add-operator '<+-> (1- +op-precedence-muldiv+) :xfy #'range)

(defmethod print-object ((r range) stream)
  (unless (quantity-p (first *print-context*))
    (princ #\{ stream))
  (print-range r stream)
  (unless (quantity-p (first *print-context*))
    (princ #\} stream)))

(defun print-range (r stream)
  (let ((m  (if (rationalp r.mean) (coerce r.mean 'float) r.mean))
        (s  (if (rationalp r.spread) (coerce r.spread 'float) r.spread)))
  (format stream "~S <+-> ~S" m s)))
