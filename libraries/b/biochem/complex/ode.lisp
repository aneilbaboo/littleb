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



;;; $Id: ode.lisp,v 1.12 2008/09/06 00:23:08 amallavarapu Exp $
;;; Description:  Extends the complex-reaction-type and complex-species-type objects to support ode modeling

(in-package #I@file)

(include-declaration :use-packages mallavar-utility)

(include @>/util :use)
(include (@>/biochem
          @>/math/ode-var
          @library/biochem/ode))

(include-documentation :description "Provides extensions to the b/biochem package which enable ODE models to be built."
                       :authors ("Aneil Mallavarapu"))

;;;
;;; For computing the ODE rates:
;;;
(defprop complex-reaction-type.rate-fn (:documentation "A function or function name which compute the rate of a reaction of this type"))

(defprop complex-reaction-type.rate-calculator (:documentation "A list capturing the user's input to .SET-RATE-FUNCTION"))

(defprop complex-reaction-type.k
    (dictionary :#= [dictionary] :relevance t
                :documentation "A dictionary of named constants referenced by the rate-fn property"))

(defield complex-reaction-type.set-rate-function (fn &rest args)
  (let* ((entities         object.lhs.vars)
         (stoichiometries  (make-list (length entities) :initial-element 1))
         (dimensions       (mapcar #'entity-dimension entities)))
   (setf .rate-calculator (list* fn args)
         .rate-fn (fix-complex-rate-expr
                   (funcall fn
                            args
                            {(location-class-dimension .location-class)
                             * *molecular-amount-dimension* 
                             / *time-dimension*}
                            .k
                            entities 
                            stoichiometries
                            dimensions)))))
  
(defun fix-complex-rate-expr (x) x)
(defield reversible-complex-reaction-type.set-rate-function (fn &key fwd rev)
  (values .fwd.(apply :set-rate-function fn (ensure-list fwd))
          .rev.(apply :set-rate-function fn (ensure-list rev))))

(defun entity-dimension (e)
  (etypecase e
    (localization (entity-dimension e.entity))
    (complex-graph-concept (location-class-dimension e.location-class))))

(defrule complex-reaction-set-rate-fn
  "Copies the rate information over from the complex-reaction-type to the reaction-type when a complex-reaction-inference is detected."
  (:and [complex-reaction-inference ?cr ?rtype ?matches]
   (?rate-fn ?cr.rate-fn))
  =>
  ?rtype.k.(_copy-from ?cr.k)
  (dolist (match ?matches)
    {?rtype.k.,(car match) := (cdr match)})
  {?rtype.rate-fn := ?rate-fn})

(defprop complex-pattern.t0 ())

(defrule initialize-complex-species-type-t0 
  (:and [complex-pattern-match ?cp ?cst ?iso]
        (?t0 ?cp.t0))
  =>
  {?cst.t0 := ?t0})

