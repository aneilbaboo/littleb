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


;;; File: Var
;;; Description: Vars - named variables for building ODE models

;;; $Id: ode-var.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $
;;;
(in-package #I@FILE)

(include (@FOLDER) :expose)

(include @LIBRARY/units/dimensionalization :use)

;;;
;;; ode-var, a time-variant var
;;; 
(defun compute-ode-var-rate-assign-dimension-error (cause)
 (lambda (var val)
   (b-error "Assigning {~S.(rate-of ...) := ~S}.  Invalid dimension (~S) computed by ~S.  Expecting a quantity of ~S."
            var {val / *time-dimension*} {(dimension-of val) / *time-dimension*} cause {var.dimension / *time-dimension*})))

(defcon ode-var (:notrace base-var) ;; a time-variant value
  (&optional (id := *name*)
   &property (init-cond) ; the initial-condition
             (rate-influences := nil))) ; an assoc list of cons pairs: (CAUSE . INFLUENCE)

(defield ode-var.t0 ()
  (if (property-bound-p object :init-cond) .init-cond
    (quantity 0 .dimension)))

(defield (setf ode-var.t0) (value)
  (setf .init-cond (check-var-numeric-field-with-restarts object :t0 value)))

(defield ode-var.rate-of (&rest cause)
  (tvar-rate-from-influence-pair object (assoc cause .rate-influences :test #'equalp)))

(defun tvar-rate-influence-substitution-fn (value)
  (typecase value
    (math-expression value.(map #'tvar-rate-influence-substitution-fn))
    (numeric         value)
    (function        (let ((val (funcall value)))
                       (if (math-expression-p val)
                           val.(map #'tvar-rate-influence-substitution-fn)
                         val)))
    (t               value)))

(defun tvar-rate-from-influence-pair (tvar influence-pair)
  (let* ((cause   (car influence-pair))
         (value   (cdr influence-pair))
         (rate    (tvar-rate-influence-substitution-fn value)))
    (unless (eq (dimension-of tvar)
                (dimension-of (*op (quantity 0 *time-dimension*) rate)))
      (invalid-ode-var-influence-rate tvar cause rate))
    rate))

(defun invalid-ode-var-influence-rate (tvar cause rate)
  (b-error "Invalid rate ~S caused by ~S: expecting dimension ~S, but calculated ~S." 
           rate cause ;tvar
           (dimension-of {tvar / (|DIMENSION.UNIT| *time-dimension*)})
           (dimension-of rate)))

(defield (setf ode-var.rate-of) (value &rest cause)
  (unless (functionp value)
    (var-check-dimension object (*op (quantity 0 *time-dimension*) value) 
                         (compute-ode-var-rate-assign-dimension-error cause)))
  (let ((existing (assoc cause .rate-influences :test #'equalp)))
    (if existing
        (setf (cdr existing) value)
      (push (cons cause value) .rate-influences))))

(defield ode-var.rate ()
  (fast-add (mapcar (lambda (pair)
                      (tvar-rate-from-influence-pair object pair)) .rate-influences)))

(defmethod set-default-field ((tv ode-var) val)
  (setf tv.t0 val))
