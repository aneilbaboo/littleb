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

;;; File: non-dimensionalize
;;; Description: Defines WITH-NON-DIMENSIONAL-MATH.  Provides support for
;;;      the automatic reduction of dimensional expressions to non-dimensional-value ones.

(in-package b/math)

(include-documentation
  :description "Non-dimensionalization system WITH-DIMENSIONLESS-MATH and other macros and functions."
  :institution "Harvard Medical School"
  :author "Aneil Mallavarapu")

(defparameter *non-dimensionalizing-fn* ()
  "A function of one argument which returns two values: (VAL, SUCCESS), if success is non-NIL,
VAL represents a non-dimensionalized form of the input argument")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro force-dimensional-math (&body body)
  "Used by operators to force a bypass of the non-dimensionalizing system."
  `(let ((*non-dimensionalizing-fn* nil))
     ,@body))
(expose-symbol '(with-dimensionless-math force-dimensionless-math *non-dimensionalizing-fn*
                 dimensionless non-dimensionalize-p dimensionalize))

(defmacro with-dimensionless-math (method &body body)
  "Causes arithmetic and print operations in BODY to convert their arguments to non-dimensional-value
form (as by non-dimensional-value).  The METHOD parameter (evaluated) must be a (possibly empty) list
of quantities or units, or a function.  The default non-dimensionalizing method (indicated by the
empty list) uses the base units of the base dimensions.  This method is guaranteed to be successful.
   For example, (with-dimensionless-math () {1 millijoules}) is equivalent to 
                (with-dimensionless-math (meters seconds kilograms) {1 millijoules}), and
   will yield a value of 1/1000.

   When a non-NIL list is provided, it is consulted to determine whether a specific unit or quantity
has been provided for non-dimensionalizing a particular dimension.  If not, the method provided by the
innermost dynamically enclosing with-dimensional-math block is used.  If that also fails, the default
non-dimensionalizing method is used.

   When a function is provided, it is called with the object to be non-dimensionalized.  The function
should return either NIL (indicating failure) or a value which has the same dimension as the argument.
If NIL is returned, the method provided in the dynamically enclosing with-dimensional-math block is used

   Note: The value returned by with-dimensionless-math is not necessarily non-dimensional: only mathematical
operations are affected by this macro.  For example, evaluating a symbol bound to a quantity within a
lexically defined with-dimensionless-math block will result in a quantity, not a number:      
             (define my-reference-var [[reference-var] :value {1 joules}]) => my-reference-var
             (with-dimensional-math () (dimension-of my-reference-var)) => energy"
  `(let* ((*non-dimensionalizing-fn* (calculate-non-dimensionalizing-fn ,method)))
     ,@body))
)


(defun calculate-non-dimensionalizing-fn (method)
  (typecase method
    (list     (calculate-list-non-dimensionalize-fn method))
    (function (calculate-fn-non-dimensionalize-fn method))
    (t        (b-error "Invalid non-dimensionalizing method (~S) passed to WITH-DIMENSIONLESS-MATH." method))))

(defun non-dimensionalizing-value-from-base-units (o &optional map)
  (flet ((compute-base-nd-val (base-dim)
           (ifit (assoc base-dim map) (cdr it) (quantity 1 (|DIMENSION.UNIT| base-dim)))))
   (force-dimensional-math
     (let ((d (dimension-of o)))
       (etypecase d
         (base-dimension      (compute-base-nd-val d))
         (derived-dimension   (loop for de in (|DERIVED-DIMENSION.BASE-DIMENSIONS| d)
                                    for db = (dimension-element-base de)
                                    for dp = (dimension-element-power de)
                                    for base-nd-val = (compute-base-nd-val db)
                                    for nd-val = (^op base-nd-val dp)
                                    then      (*op nd-val (^op base-nd-val dp))
                                    finally (return nd-val))))))))

(defun calculate-list-non-dimensionalize-fn (list)
  (let ((map (mapcar (lambda (o)
                       (cons (dimension-of o) 
                             (if (unit-p o) (quantity 1 o) o)))
                     list))
        (prev-nd-fn *non-dimensionalizing-fn*))
    (lambda (o)
      (ifit (assoc (dimension-of o) map) (cdr it)
        (or (if prev-nd-fn (funcall prev-nd-fn o))
            (non-dimensionalizing-value-from-base-units o map))))))

(defun calculate-fn-non-dimensionalize-fn (fn)
  (let ((prev-nd-fn *non-dimensionalizing-fn*))
    (lambda (o)
      (or (funcall fn o)
          (if prev-nd-fn (funcall prev-nd-fn o))
          (non-dimensionalizing-value-from-base-units o)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(declaim (inline non-dimensionalize-p))
(defun non-dimensionalize-p ()
  "Returns a non-nil value if non-dimensionalization is required."
  (if *non-dimensionalizing-fn* t)))

(defun dimensionless (o)
  "Ensures that an appropriately non-dimensionalized object is returned.  This function uses
   the current non-dimensionalizing method to calculate an appropriate non-dimensional
   value.  "
  (if (non-dimensionalize-p)
      (non-dimensional-value o) ; no need to recalc *non-dimensionalizing-fn*
    (with-dimensionless-math () (non-dimensional-value o)))) ; otherwise, force calculation of the fn

(defun dimensionalize (val dim)
  "Ensures that val is a unit in the appropriate dimension.  When a dimensional value is needed within a with-dimensionless-math block (e.g., when assigning to a place which expects a dimensional value)"
  (cond
   ((non-dimensionalize-p) (*op (/op val (non-dimensionalizing-value val))
                                (non-dimensionalizing-value (quantity 1 dim))))
   (t (quantity 1 dim))))

(defun non-dimensional-value (o)
  (typecase o
    (dimension o)
    (t  (let ((nd-val (non-dimensionalizing-value o)))
          (force-dimensional-math 
            (/op o nd-val))))))

(defun non-dimensionalizing-value (o)
  "Used internally: when called dynamically inside with-dimensionless-math, returns the
value to be used to non-dimensionalize an object o"
  (funcall *non-dimensionalizing-fn* o))

;;;
;;; operators
;;;
(defmethod ^op :around (b p)
  (if (non-dimensionalize-p)
        (let ((nd-b (non-dimensional-value b))
              (nd-p (non-dimensional-value p)))
          (force-dimensional-math (^op nd-b nd-p)))
    (call-next-method)))

(define-condition dimension-combination-error (b-error)
  ((argument1 :initarg :argument1 :reader dimension-combination-error-argument1)
   (argument2 :initarg :argument2 :reader dimension-combination-error-argument2)
   (operator :initarg :operator :reader dimension-combination-error-operator))
  (:default-initargs :format-string "~S ~A ~S => ~S ~A ~S: cannot combine values with different dimensions."))

(defun dimension-combination-error-dimension1 (dce)
  (dimension-of (dimension-combination-error-argument1 dce)))
(defun dimension-combination-error-dimension2 (dce)
  (dimension-of (dimension-combination-error-argument2 dce)))

(defmethod print-object ((e dimension-combination-error) stream)
  (with-slots (argument1 argument2 operator format-arguments) e
    (setf format-arguments
          (list argument1 operator argument2
                (dimension-of argument1)
                operator
                (dimension-of argument2)))
    (call-next-method)))
                  
(defun dimension-combination-error (arg1 arg2 op)
  (error 'dimension-combination-error :argument1 arg1 :argument2 arg2 :operator op))

(defun check-dimensions (a1 a2 op)
  (let* ((d1 (dimension-of a1))
         (d2 (dimension-of a2)))
    (when (and d1 d2 (not (eq d1 d2)))
      (dimension-combination-error a1 a2 op))))

(defmethod +op :around (o1 o2)
  (cond
   ((non-dimensionalize-p)          (let ((nd-o1 (non-dimensional-value o1))
                                               (nd-o2 (non-dimensional-value o2)))
                                           (force-dimensional-math (+op nd-o1 nd-o2))))

   (t                                (check-dimensions o1 o2 '+)
                                     (call-next-method))))

(defmethod *op :around (o1 o2)
  (cond
   ((non-dimensionalize-p)          (let ((nd-o1 (non-dimensional-value o1))
                                          (nd-o2 (non-dimensional-value o2)))
                                      (force-dimensional-math (*op nd-o1 nd-o2))))
   (t                               (call-next-method))))
