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

;;; File: 
;;; Description: Defines the Dimension concept, part of the quantity system.

;;; $Id: dimension.lisp,v 1.2 2007/10/25 14:44:23 amallavarapu Exp $
;;;
(in-package b/math)

(include-documentation
  :description "Defines concepts for describing physical dimensions (e.g., distance, time, mass) UNIT-SYSTEM, DIMENSION, BASE-DIMENSION and DERIVED-DIMENSION"
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

;;; DIMENSIONS:
(defcon unit-system (:notrace)
  (&optional (id := *name*)))

(defcon dimension (:abstract :notrace :non-matchable math-concept) 
  (&property (scale-units list := () :relevance :force)
             (unit-system (allow unit-system) := ())))

(defun dimension-add-scale-unit (d u)
  (with-relevance :force
    (pushnew u d.scale-units)))

(defield dimension.select-units (&optional usys)
  "Returns a list of all units available for the unit system specified.  Default is the unit system of the dimension."
  (let ((usys (or usys unit-system)))
    (remove-if #'null (mapcar (lambda (u) (when (and (property-bound-p u :unit-system)
                                                     (eq usys u.unit-system)) u))
                              .scale-units))))


(defield dimension.calculate-unit (base-units)
  (calculate-unit object base-units))

(defcon base-dimension (:non-matchable :notrace dimension)
  (&optional (id := *name*)))

(defield base-dimension.base-dimensions ()
  (list (cons object 1)))

(defield dimension.nearest-scale-unit (magnitude)
  (dimension-nearest-scale-unit object magnitude))


;;;; (DSPEC (DEFINE NULL-DIMENSION)
;;;;   (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;;;     (EXPORT 'NULL-DIMENSION)
;;;;     (DEFINE-GLOBAL NULL-DIMENSION
;;;;                    (KB-TRANSACTION (SETF GLOBAL-VALUE::B.<NULL-DIMENSION> (has-name NULL-DIMENSION (OBJECT BASE-DIMENSION)))))))

(define null-dimension [base-dimension])

(defun no-dimension-p (o)
  "True if o is NIL or the NULL-DIMENSION"
  (or (null o) (eq o null-dimension)))
         
(defun check-valid-dimension-element (de)
  (let ((dp (dimension-element-power de))
        (db (dimension-element-base de)))
    (assert (and (not (floatp dp))
                 (no-dimension-p (dimension-of dp))
                 (base-dimension-p db)) ()
      "Invalid dimension-element: ~S" de)))

(defcon derived-dimension (:non-matchable :notrace dimension)
  ((base-dimensions list)) ;; ((BASE-DIM1 . POW1) (BASE-DIM2 . POW2)...)
  (mapc #'check-valid-dimension-element .base-dimensions))


(defun select-power-pairs (power-list &optional p)
  "Given a list of the form ((X1 . P1) (X2 . P2)...), returns all pairs which match P.  
P may be an integer, or :+ (select all positives) or :- (select all negatives)."
  (case p
    (:+ (remove-if (lambda (o) (< (cdr o) 0)) power-list))
    (:- (remove-if (lambda (o) (> (cdr o) 0)) power-list))
    (t (remove-if (with #!eq p) power-list
                  :key #'car))))

(defmethod dimension-select-base-dimensions ((d derived-dimension) &optional p)
  "Depending on p, returns dimension-power cons pairs"
  (select-power-pairs d.base-dimensions p))

(defmethod dimension-select-base-dimensions ((d base-dimension) &optional p)
  (case p
    (:+ (list (cons d 1)))
    (:- null-dimension)
    (t  (list (cons d 1)))))


(defun dimension-numerator (d)
  (make-dimension-from-dimension-power-list 
   (dimension-select-base-dimensions d :+)))

(defun dimension-denominator (d)
  (make-dimension-from-dimension-power-list 
   (mapcar (lambda (dp)
             (cons (car dp) (abs (cdr dp))))
           (dimension-select-base-dimensions d :-))))

(defun dimension-element (base &optional (power 1)) 
  (cons base (simplify-number power)))
(defun dimension-element-base (de) (car de))
(defun dimension-element-power (de) (cdr de))
(defun (setf dimension-element-power) (value de) (setf (cdr de) value))

(defun combine-dimensions (d1 d2)
  (flet ((dim-elt-list (d) 
           (if (base-dimension-p d) 
               (list (dimension-element d))
             (|DERIVED-DIMENSION.BASE-DIMENSIONS| d))))
    (cond
     ((eq null-dimension d1) d2)
     ((eq null-dimension d2) d1)
     (t (make-dimension-from-dimension-power-list
         (remove-if (lambda (o) (and (numberp o) (zerop o)))
                    (combine-var-value-lists (dim-elt-list d1) (dim-elt-list d2))
                    :key #'dimension-element-power))))))

(defun make-dimension-from-dimension-power-list (base-dimensions)
  (flet ((is-base-dim (dim-list)
           (and (= 1 (length dim-list))
                (eql (cdr (first dim-list)) 1)))
         (simplify-dim-elt (de)
           (let* ((power   (dimension-element-power de)))
             (when (math-expression-p power)
               (let ((epower  (if (math-expression-p power)
                                  (expand power) 
                                power)))
                 (if (unit-sum-expression-p epower)
                     (with-multiplicity-sum-expression epower (val coef) 
                       (declare (ignorable coef))
                       (setf (dimension-element-power de) val))
                   (setf (dimension-element-power de) epower)))))))
    (mapc #'simplify-dim-elt base-dimensions)
    (cond
     ((null base-dimensions)                          null-dimension)       
     ((is-base-dim base-dimensions)                   (dimension-element-base (first base-dimensions)))
     (t                                               [derived-dimension base-dimensions]))))
  

;;;
;;; printing
;;;
(defmethod print-concept ((d base-dimension) &optional (stream *standard-output*))
  (cond
   (*debug-printing* (call-next-method))
   (*print-context*  (prin1 d.id stream))
   (t                (call-next-method))))

(defmethod print-concept ((d derived-dimension) &optional (stream *standard-output*))
  (cond 
   (*debug-printing* (call-next-method))
   (t (with-print-context d
        (print-math-expression (product-expression d.base-dimensions) stream)))))

;;;; Remove: old derived-dimension print code
;;;; (defmethod print-concept ((d derived-dimension) &optional (stream *standard-output*))
;;;;   (cond
;;;;    (*debug-printing* (call-next-method))
;;;;    (t (pprint-logical-block (stream nil :prefix "{" :suffix "}")
;;;;         (print-power-pairs d.base-dimensions stream)))))
   
;;;
;;; DIMENSION PRINTING SYSTEM:
;;;
(defun nearest-exact-scale-unit (dim mag)
  (let* ((best-unit  nil)
         (least-diff most-positive-double-float)
         (logmag     (if (equalp mag 0) (log 1) (log mag))))
    (mapcar (lambda (u)
              (let ((new-diff (abs (- (log u.conversion) logmag)))
                    (usys     (if (property-bound-p u :unit-system) u.unit-system)))
                (if (and (< new-diff least-diff)
                         usys (eq usys dim.unit-system))
                    (setf least-diff new-diff
                          best-unit u))))
            dim.scale-units)
    best-unit))

(predefine (:class unit)
(defun nearest-base-scale-units (dim mag)
  "Returns a unit-power list corresponding to dim, where the first unit is scaled as close as possible to mag."
  (flet ((check-dim-has-units (dim)
           (assert dim.scale-units () "No scale units defined for base dimension ~S" dim)))
    (assert (derived-dimension-p dim) () "Expecting a derived dimension here, but received ~S" dim)
    (let* ((dp-list   (sort (copy-tree dim.base-dimensions) ; must copy - sort is destructive
                            (lambda (o1 o2)
                              (and (numberp o1) (numberp o2)
                                   (> o1 o2))) :key #'cdr))
           (main-dp   (first dp-list))
           (bdim      (car main-dp))
           (power     (cdr main-dp))
           (main-unit (nearest-exact-scale-unit bdim (if (= mag 0) 0
                                                       (careful-expt mag (/ 1 power))))))
      (check-dim-has-units bdim)
      (cons (cons main-unit power)
            (mapcar (lambda (dp)
                      (cons [unit (car dp) 1.0] (cdr dp)))
                    (rest dp-list)))))))          

(defun dimension-nearest-scale-unit (dim mag)
  "May return a single unit of a derived dimension or a list
of base units, where the first is scaled appropriately"
  (cond
   ((symbolic-dimension-p dim)
    (|DIMENSION.UNIT| dim))
   (t
    (or (nearest-exact-scale-unit dim mag)
        (nearest-base-scale-units dim mag)))))


(defun symbolic-dimension-p (d)
  (and (derived-dimension-p d)
       (some (compose #'math-expression-p #'dimension-element-power)
             (|DERIVED-DIMENSION.BASE-DIMENSIONS| d))))



(defun calculate-unit (dim base-units)
    (flet ((calc-base-unit (dp bdus)
             (let* ((dim (car dp))
                    (pow (cdr dp))
                    (u   (or (find dim bdus :key ?.dimension)
                             dim.unit)))
               (^op u  pow)))

           (normalize-base-unit (u)
             (let ((dim u.dimension))
               (cond
                ((base-dimension-p dim) u)

                ((and (derived-dimension-p dim)
                      (= (length dim.base-dimensions) 1))
                 (let* ((bdp (first dim.base-dimensions))
                        (pow (cdr bdp)))
                   (^op U (/op 1  pow))))

                (t (error "Invalid input to CALCULATE-UNIT:~S" base-units))))))
               
                    
      (let* ((dims  dim.base-dimensions)
             (bdus  (mapcar #'normalize-base-unit base-units))
             (units (mapcar (lambda (dp) 
                              (calc-base-unit dp bdus)) dims)))
        (if (> (length units) 1)
            (reduce #'*op units)
          (first units)))))

(defmethod magnitude-of ((d dimension) &optional u) null-dimension)
(defmethod dimension-of ((n number)) null-dimension)
(defmethod dimension-of ((d dimension)) null-dimension)
(defmethod dimension-of ((o t)) null-dimension)
(defmethod fld ((o number) (field (eql :dimension)) &rest args) null-dimension)

