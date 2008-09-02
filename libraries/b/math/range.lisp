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

;;; File: range
;;; Description: defines a range of values
;;;  not working - don't understand the semantics for exponentiation 

;;; $Id: range.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $

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
