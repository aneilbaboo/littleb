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

;;; File: quantity
;;; Description: Defines the Quantity concept, a representation of a dimensional
;;;              quantity (such as 5 joules).

;;; $Id: quantity.lisp,v 1.1 2007/09/25 17:54:05 amallavarapu Exp $
;;;
(in-package b/math)

;;; QUANTITIES:
(defcon quantity (:non-matchable numeric-concept)
  (base-magnitude 
   dimension))

(define null-unit [unit null-dimension])
(setf null-dimension.unit null-unit)

(defield quantity.in (u)
  "Returns the magnitude of quantity the units provided, where U = a unit object."
  (if (eq u.dimension object.dimension) (float (convert-to-unit object.base-magnitude u nil))
    (error "Dimension mismatch ~S does not have same dimension as ~S" object u)))

(defun quantity (magnitude kind)
  (let* ((mag (if (floatp magnitude) 
                  (rationalize magnitude)
                magnitude)))
    (cond
     ((or (eq kind null-unit)       
          (null kind)    
          (eq kind null-dimension)) mag)
              
     ((unit-p kind)                [quantity (convert-to-unit mag kind t) (|UNIT.DIMENSION| kind)])
              
     ((dimension-p kind)           [quantity mag kind])
              
     (t                           (error "(~S ~S ~S) is invalid." 'quantity magnitude kind)))))

(defun quantity-denominator (q)
  (typecase q
    (quantity   (quantity (denominator (|QUANTITY.BASE-MAGNITUDE| q))
                          (dimension-denominator (|QUANTITY.DIMENSION| q))))

    (dimension  (dimension-denominator q))

    (number     (denominator q))
    
    (t          1)))

(defield quantity.nearest-unit (&optional (allow-inverse nil))
  (find-best-units object allow-inverse))

(defmethod fld ((o number) (field (eql :nearest-unit)) &rest args)
  (declare (ignorable args field))
  null-unit)

(defun quantity-numerator (q)
  (typecase q
    (quantity   (quantity (numerator (|QUANTITY.BASE-MAGNITUDE| q))
                          (dimension-numerator (|QUANTITY.DIMENSION| q))))
    (dimension  (dimension-numerator q))
    (number     (numerator q))
    (t          1)))

(defun is-zero (o)
  (and (numberp o) (zerop o)))

(defun zero-quantity-p (o)
  (and (quantity-p o)
       (equalp 0 (|QUANTITY.BASE-MAGNITUDE| o))))

(defun zero-numeric-p (o)
  (or (equalp 0 o)
      (zero-quantity-p o)))
;;;
;;; Printing
;;;     
(defmethod print-object ((q quantity) stream)
  (cond
   (*debug-printing*                   (print-concept q stream))
   (t                                  (print-math-expression q stream))))

(defmethod print-math-expression ((q quantity) &optional (stream *standard-output*) (lhs-precedence t))
  (cond
   ((non-dimensionalize-p)             (print-math (dimensionless q) stream))

   (t 
    (let ((braces?  (braces-required-p lhs-precedence '*)))
      (with-print-context q
        (pprint-math-block (stream braces?)
          (let+ (((u m inv) (find-best-units q)))
            (print-math m stream)            
            (print-unit-product u inv t '* stream))))))))


(defun find-best-units (q &optional (allow-inverse t))
  "Returns 3 values: (BESTU, BESTM, INV) indicating the best unit, the best magnitude,
 and whether the units represent the inverse of the actual units"
  (flet ((best-base-units (bestu mag)
           (let* ((main-unit  (car (first bestu)))
                  (main-power (cdr (first bestu)))
                  (bestm      (convert-to-unit mag main-unit nil main-power)))
             (values bestu bestm nil))))

    (let* ((dim     (|QUANTITY.DIMENSION| q))
           (mag     (|QUANTITY.BASE-MAGNITUDE| q))
           (bestu   (dimension-nearest-scale-unit dim (magnitude-of mag))))

      (cond
       ;; if we find a best simple unit in the current dimension        
       ((and bestu (atom bestu)) 
        (values bestu (convert-to-unit mag bestu) nil))
     
        ;; 2nd choice, if we find a best simple unit in the inverse dimension
       ((and (not (equalp mag 0)) allow-inverse) ; otherwise, look for a best unit
        (let* ((inv-mag   (/op 1 mag))           ;  in the inverse dimension
               (inv-dim   [(^op dim -1) :unit-system (if object.unit-system object.unit-system
                                                       dim.unit-system)])
               (best-invu (dimension-nearest-scale-unit inv-dim (magnitude-of inv-mag))))
                  
          (if (and best-invu (atom best-invu))
              (let ((best-invm (/op 1 (convert-to-unit inv-mag best-invu))))
                     ; return the inverse unit & mag, 
                (values best-invu best-invm t))
                ;; otherwise give answer in terms of base units
            (best-base-units bestu mag))))

       ;; last choice is to give the answer in terms of base units:
       (t        (best-base-units bestu mag))))))

(defmethod dimension-of ((q quantity)) (the quantity q).dimension)
(defmethod magnitude-of ((q quantity) &optional u)
  (let* ((m (|QUANTITY.BASE-MAGNITUDE| q)))
    (cond 
     ((eq nil u)                m) ; unit unspecified, use base unit
     ((unit-p u)                (values (convert-to-unit (|QUANTITY.BASE-MAGNITUDE| q) u) u))
     (t                         (error "Cannot determine MAGNITUDE-OF ~S in unit ~S" q u)))))

(defmethod negativep ((q quantity))
  (negativep q.base-magnitude))

(hide-classes quantity)
