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

;;; File: Unit
;;; Description: Defines the unit concept. For supporting Joules, Newtons, etc.
;;;              

;;; $Id: unit.lisp,v 1.4 2008/09/02 14:58:10 amallavarapu Exp $
;;;
(in-package b/math)

(include-documentation
  :description "Defines the unit concept. For supporting Joules, Newtons, etc."
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; UNITS: - the conversion parameter is either:
;;;          NIL or 1, indicating that this is the base-unit
;;;          A number, indicating a multiplier of the base-unit of the dimension
;;;          A binary function where the first arg is a magnitude and the second
;;;          number is a flag indicating the direction of the desired conversion
;;;          (NIL = from base-units to this unit, T = from this unit to base-units)
(defcon unit (:notrace :non-matchable math-concept) 
  ((dimension dimension)
   &optional (conversion := 1)
   &property (unit-system unit-system))

  (flet ((calc-conversion (dim c)
           (if (functionp .conversion) 
               (lambda (v inv)
                 (if inv (/ v (funcall .conversion dim inv))
                   (* v (funcall .conversion dim inv))))
             (* c .conversion))))

    (multiple-value-bind (dim conv)
        (etypecase .dimension
          (unit
           (let* ((u .dimension)
                  (d u.dimension))
             (values d (calc-conversion d u.conversion))))
          (quantity
           (let* ((q .dimension)
                  (d (|QUANTITY.DIMENSION| q)))
             (values d (calc-conversion d q.base-magnitude))))
          (dimension  (values .dimension .conversion))
          (null       (values null-dimension 1)))
      (setf .dimension dim
            .conversion (simplify-number conv))))
  => 
  (if (numberp .conversion)
      (dimension-add-scale-unit .dimension object)))

(defprop dimension.unit (:= [unit object 1.0] :relevance t))

(defmethod convert-to-unit ((magnitude number) u &optional inverse power)
  "Converts the magnitude to or from the units specified.  If inverse is NIL, magnitude is treated as base-units converted to the units of U  When inverse is T, magnitude is treated as being in the units of the object specified by U and converted to base units.
E.g., when SECONDS are the base-unit, (unit-conversion MINUTES 1 NIL) => 1/60 
                                  and (unit-conversion MINUTES 1 T) => 60"
  (rationalize (let ((conv u.conversion))
                 (etypecase conv
                   (null     magnitude)
                   (number   (let ((pconv (if power (careful-expt conv power) conv)))
                               (if inverse (* magnitude pconv)
                                 (/ magnitude pconv))))
                   (function (when power (b-error "Cannot convert to a power of this type of unit ~S" u))
                             (funcall conv magnitude inverse))))))

(defconstant *major-orders-of-magnitude*
  '(:milli :kilo :mega :micro :nano :giga :tera :pico :femto :atto :zepto :yocto nil))

(defconstant *all-orders-of-magnitude* 
  (append '(:deka :deci :hecto :centi) *major-orders-of-magnitude*))


(portable:define-dspec-class defMetricUnit nil nil)
(expose-symbol 'defmetricunit)
(defmacro defMetricUnit (symbol dimension unit-system 
                                &key (base-order nil) 
                                (orders '*major-orders-of-magnitude*))
  (labels ((def-scaled-unit (order)
             (let* ((prefix         (or order ""))
                    (scaled-unit    (sym prefix symbol)))
               `(define ,scaled-unit [[unit ,dimension ,(order-to-scaling-factor order)]
                                     :unit-system ,unit-system])))

           (order-to-scaling-factor (o)
             (careful-expt 10 (- (order-to-exponent o) (order-to-exponent base-order))))

           (order-to-exponent (o)
             (ecase o
               (:deka 1)    (:deci -1) 
               (:hecto 2)   (:centi -2) 
               (:kilo 3)    (:milli -3)  
               (:mega 6)    (:micro -6) 
               (:giga 9)    (:nano -9)   
               (:tera 12)   (:pico -12)
               (:peta 15)   (:femto -15) 
               (:exa 18)    (:atto -18)  
               (:zetta 21)  (:zepto -21) 
               (:yocto -24) (:yotta 24)
               ((nil) 0))))
    (let ((body `(prog1 ,(def-scaled-unit base-order)
                   ,@(mapcar #'def-scaled-unit 
                             (remove base-order 
                                     (if (symbolp orders) 
                                         (symbol-value orders)))))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (portable:dspec (defmetricunit ,symbol) ,body)))))
       
       

(defun base-unit-p (u)
  (and (equalp (|UNIT.CONVERSION| u) 1)
       (base-dimension-p (|UNIT.DIMENSION| u)))) 

(defun not-fancy-print-unit-p (u)
  (when (or *debug-printing* 
      (functionp (|UNIT.CONVERSION| u))
      (and (base-unit-p u)
           (base-dimension-p (|UNIT.DIMENSION| u))))
    t))

(defmacro pprint-unit-ctor ((stream) &body body)
  `(pprint-logical-block (,stream nil :prefix "[" :suffix "]")
     (prin1 'unit ,stream)
     (princ #\space ,stream)
     ,@body))

(defmethod print-concept ((u unit) &optional (stream *standard-output*))
  (cond 
   ((not-fancy-print-unit-p u)    (call-next-method))

   ((and u._name
         (not (= (|UNIT.CONVERSION| u) 1)))
    (print-unit-as-multiple-of-dimension-unit u stream))

   (t  (print-unit-as-function-of-base-units u stream))))

(defun print-unit-as-multiple-of-dimension-unit (u stream)
  (pprint-unit-ctor (stream)
    (pprint-math-block (stream t)
      (print-math (|UNIT.CONVERSION| u) stream)
      (print-math '* stream)
      (with-print-context t
        (print-math u.dimension.unit stream)))))

(defun print-unit-as-function-of-base-units (u stream)
  (let ((base-units-pe  (product-expression 
                         (mapcar (lambda (de)
                                   (product-element  (dimension-element-base de).unit
                                                     (dimension-element-power de)))
                                 u.dimension.base-dimensions))))
    (with-print-context u
      (if (= u.conversion 1) (print-math base-units-pe stream t)
        (pprint-unit-ctor (stream)
          (pprint-math-block (stream t)
            (print-math u.conversion stream)
            (print-math '* stream)
            (print-math base-units-pe stream '*)))))))

(defun print-unit-product (u inv left-obj lhs-prec stream)
  (let ((*print-default-operator* nil))
    (cond 
     ;; multiple base units
     ((consp u)
      (let ((pes (if inv (invert-product-elements u) u)))
        (cond
         ;; if no units in the numerator:
         ((all-product-elements-in-denominator-p pes)
          (cond 
           (left-obj (print-math '/ stream)
                     (print-math (product-expression (invert-product-elements pes)) stream 
                                 (1- (operator-precedence '/))))
           (t        (print-math (product-expression pes) stream 
                                 lhs-prec))))

         ;; there is at least one unit in the numerator
         (t (if left-obj (print-math '* stream))
            (print-math (product-expression pes)
                        stream lhs-prec)))))

     ;; single (possibly derived) unit returned:
     (u ; a best unit was found
      (cond
       (left-obj (if inv (print-math '/ stream)
                   (print-math '* stream))
                 (prin1 u stream))
       (inv      (pprint-math-block (stream (braces-required-p lhs-prec '^))
                   (prin1 u stream)
                   (print-math '^ stream)
                   (prin1 -1 stream)))
       (t        (prin1 u stream)))))))

(defun all-product-elements-in-denominator-p (pes)
  (notany (compose #'positive-p #'product-element-power) pes))


(defmethod dimension-of ((u unit)) (the unit u).dimension)

)


;;;
;;; PRINT-CONCEPT method
;;;
;;;; (defmethod  print-concept ((u unit) &optional stream)
;;;;   (let ((conv u.conversion))
;;;;     (cond
;;;; ;     ((eql conv 1) (call-next-method))
;;;;      (t
;;;;       (with-print-context u
;;;;         (pprint-logical-block (stream nil :prefix "[" :suffix "]")
;;;;           (prin1 unit stream)
;;;;           (princ #\space stream)
;;;;           (prin1 u.dimension stream)
;;;; ;          (print-dimension-base-units u.dimension stream)
;;;;           (princ #\space stream)
;;;;           (princ conv stream)))))))

;;;; (defun print-unit-as-function-of-other-units (u stream)
;;;;   (with-print-context u
;;;;     (let+ (((bestm bestu inv) (find-best-units (quantity 1 u) t))
;;;;            (in-unit-ctor      (not (= bestm 1))))
;;;;       (cond
;;;;        (in-unit-ctor         (pprint-unit-ctor (stream)
;;;;                                (pprint-math-block (stream t)
;;;;                                  (print-math bestm stream)
;;;;                                  (print-math '* stream)
;;;;                                  (print-unit-product u inv nil t stream))))
;;;;        
;;;;        (t                  (print-unit-product bestu inv nil t stream))))))

