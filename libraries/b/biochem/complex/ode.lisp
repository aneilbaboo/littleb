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


;;; $Id: ode.lisp,v 1.11 2008/09/02 14:58:10 amallavarapu Exp $
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

