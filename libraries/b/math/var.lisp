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

;;; File: Var
;;; Description: Vars - named variables

;;; $Id: var.lisp,v 1.2 2008/01/22 16:42:40 amallavarapu Exp $
;;;
(in-package b/math)

(defcon var (:abstract) ())

(defun var-check-dimension (v val error-fn)
  (let ((val-dim (dimension-of val))
        (var-dim (if (property-bound-p v :dimension) v.dimension)))
    (when (and var-dim
               (not (eq val-dim var-dim)))
      (funcall error-fn v val))
    (setf v.dimension val-dim)))

(defun var-default-assign-dimension-error (var val)
  (b-error "Dimension error {~S.value := ~S} conflicts with {~S.dimension = ~S}."
           var val var var.dimension))

;;;
;;; Vars which represent base forms are of
;;;      subtype base-var:
;;;
(defcon base-var (:abstract var) 
  "A normal form variable")

(defun compute-var-restart-fn (tag)
  (lambda (new-dimension object main-field main-value &rest args)
    (setf object.dimension new-dimension
          object.,main-field main-value)
    (mapp (lambda (k v) (setf object.,k v)) args)
    (throw tag (values main-value t))))

(defun compute-var-restart-report-fn (v field val)
  (lambda (stream)
    (let ((invalid-fields (remove field (var-determine-bound-numeric-fields v))))
      (let ((*print-context* t))
        (format stream "Accept new value (~S)" val)
        (if invalid-fields
            (format stream " and enter new values for~{ ~S~}." invalid-fields)
          (princ #\. stream))))))

(defun compute-var-restart-interactive-fn (v field val &optional (new-dimension-fn #'dimension-of))
  (lambda ()
    (let ((invalid-fields (remove field (var-determine-bound-numeric-fields v))))
      `(,(funcall new-dimension-fn val)
        ,v
        ,field
        ,val
        ,@(loop for field in invalid-fields
                do (format *query-io* "Invalid dimension for ~S.~A ~S.  Enter new value: " v field v.,field)
                collect field
                collect (eval (read *query-io*))
                do (fresh-line *query-io*))))))

(defun var-determine-bound-numeric-fields (v)
  (loop for fi in v._fieldinfos
        for field-symbol = fi.symbol
        if (and (eq fi.type 'numeric)
                (eq fi.kind :property)
                (property-bound-p v field-symbol))
        collect field-symbol))

(defmacro check-var-numeric-field-with-restarts (var field value
                                                     &key (dimension-expression nil)
                                                     (error-fn 'var-default-assign-dimension-error))
  (let ((ret (gensym "RET"))
        (continue (gensym "CONTINUE"))
        (tag      (gensym "TAG"))
        (stream   (gensym "STREAM"))
        (dimension-expression (or dimension-expression value)))
    `(block ,tag
       (let+ (((,ret ,continue)
               (catch ',tag
                 (restart-bind  
                     ((:use-new-values (compute-var-restart-fn ',tag)
                       :report-function (compute-var-restart-report-fn ,var ,field ,value)
                       :interactive-function (compute-var-restart-interactive-fn ,var ,field ,value)
                       :test-function (constantly t))
                      (:keep-old-value (lambda () (throw ',tag (values (fld ,var ,field) t)))
                       :report-function (lambda (,stream) (format ,stream "Keep old value (~S)." (fld ,var ,field)))
                       :interactive-function ()
                       :test-function (constantly t)))
                   (var-check-dimension ,var ,dimension-expression #',error-fn)
                   (return-from ,tag ,value)))))
         (if ,continue (return-from ,tag ,ret))))))

(defprop base-var.dimension ()
  (cond
   ((non-dimensionalize-p) null-dimension)
   (t value)))

(defprop base-var.min (numeric)
  (check-var-numeric-field-with-restarts object :min value))

(defprop base-var.max (numeric)
  (check-var-numeric-field-with-restarts object :min value))

;;;
;;; Constant var: holds a time-invariant value:
;;;
(defcon reference-var (:notrace base-var) 
  (&optional (id := *name*)))

(defprop reference-var.value ()
  (check-var-numeric-field-with-restarts object :value value))

(defield reference-var.resolve-val ()
  (if (reference-var-p .value) (|REFERENCE-VAR.RESOLVE-VAL| .value) .value))

(defun ensure-reference-var (o)
  (if (reference-var-p o) o [[reference-var] :value o]))

;;; DERIVED-VAR - a derived-var is derived from a var, which
;;;        is called its "base", using a function (named FN)

;;; note: currently, the inverse function (INVERSE-FN) 
;;; must be provided by the user.  In principle, we 
;;; should be able to compute this by equation solving, but
;;; I don't have this functionality yet.

(defcon derived-var (:notrace var)
  ((base var) fn
   &property (inverse-fn)
             (tag)))
           
(defield derived-var.dimension ()
  (force-dimensional-math
    (cond
     ((non-dimensionalize-p) null-dimension)
     (t                      (dimension-of .fn.(let .base (quantity 0 .base.dimension)))))))

(defield derived-var.normal-base ()
  "Guarantees that a base-var will be returned by
iteratively accessing the base field."
  (if (base-var-p .base) .base
    .base.normal-base))

(defield derived-var.normal-fn ()
  "Computes the FN that translates between the DERIVED-VAR 
and the normal-base"
  (if (base-var-p .base) .fn.(let)
    .fn.(let .base .base.normal-fn)))

(defield derived-var.normal-inverse-fn ()
  (if (base-var-p .base) .inverse-fn.(let)
    .fn.(let .base .base.inverse-fn)))

(defield derived-var.value ()
  .fn.(let .base .base.value))

(defield (setf derived-var.value) (value)
  (setf .base.value .inverse-fn.(let object value)))

(defield derived-var.get-value ()
  .fn.(let .base .base.get-value))

(defield derived-var.t0 ()
  .fn.(let .base .base.t0))

(defield (setf derived-var.t0) (value)
  (setf .base.t0 .inverse-fn.(let object value)))
  
(defield derived-var.rate-of (cause)
  .fn.(let .base .base.(rate-of cause)))

(defield (setf derived-var.rate-of) (value cause)
  (setf .base.(rate-of cause) .inverse-fn.(let object value)))

(defield derived-var.rate-influences ()
  (mapcar (lambda (pair)
            (cons (first pair)
                  .fn.(let .base (cdr pair))))
          .base.rate-influences))

(defield derived-var.rate ()
  .fn.(let .base .base.rate))
      

(defun compute-var-substitution (vars preferred-derived-vars)
  "Filters a list of vars (VARS) against preferred-derived-vars and returns
a list of substitutions, a list of pairs of the form (V S): vars and 
their substitutions.  BASE-VARS are preferred by default.  A normal-base 
of a preferred DERIVED-VAR is substituted for the DERIVED-VAR.  
This function may be used to generate a set of vars in a preferred format, 
e.g., DERIVED-VARS representing concentrations rather than ODE-VARS representing amounts."
  (flet ((preferred (x) (find x preferred-derived-vars)))
    (loop for v in vars
          for derived-var-sub = (if (base-var-p v)
                             (find v preferred-derived-vars :key ?.base-var))
          ;; non-preferred DERIVED-VARs are substituted for their base-var
          when (and (derived-var-p v) (#!preferred v))
          collect (list v v.normal-base)

          ;; Base-Vars are substituted if a matching preferred DERIVED-VAR exists
          when derived-var-sub
          collect (list v derived-var-sub))))

(defmethod dimension-of ((v var))
  v.dimension)


;;;
;;; The following code allows setting of default fields using the :@= operator
;;;     this operator can be extended to include other objects with default fields.
;;;

(defgeneric set-default-field (obj val)
  (:documentation "Used by the :@= operator to assign val to a default field of the object, obj"))

(defmethod set-default-field ((cv reference-var) val)
  (setf cv.value val))

(defmethod set-default-field ((dv derived-var) val)
  (set-default-field dv.base dv.inverse-fn.(let dv val))
  val)

(defun carefully-set-default-field (obj fld val)
  (let* ((fld-val  (if (field-bound-p obj fld) obj.,fld))
         (method   (if fld-val
                       (find-method #'set-default-field () 
                                    (list (class-of fld-val) #.(find-class t)) nil))))
    (cond
     (method (set-default-field fld-val val))
     (t      (setf (fld obj fld) val)))))
    
(defmacro set-default-field-parser (lhs rhs)
  (cond 
   ((fld-form-p lhs)  `(carefully-set-default-field ,(fld-form-object lhs) ,(fld-form-field lhs) ,rhs))
   (t                 `(set-default-field ,lhs ,rhs))))

(add-operator :@= +op-precedence-assignment+ :xfy 'set-default-field-parser)
