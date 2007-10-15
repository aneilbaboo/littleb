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

;;; File: reaction-ode.lisp
;;; Description:  Extends the reaction and reaction-type objects to support ode modeling

;;; $Id: ode.lisp,v 1.2 2007/10/15 12:48:50 amallavarapu Exp $

(in-package #I@FILE)

(include-declaration :use-packages mallavar-utility)

(include @>/util :use)
(include (@>/biochem
          @>/units/dimensionalization
          @>/math 
          @>/math/ode-var
          @</reaction-inference) :expose)

(include-documentation :description "Provides extensions to the b/biochem package which enable ODE models to be built."
                       :authors ("Aneil Mallavarapu"))
;;;
;;; For computing the ODE rates:
;;;
(defprop reaction-type.rate-fn (:documentation "A function or function name which compute the rate of a reaction of this type"))

(defprop reaction-type.k (dictionary :#= [dictionary] :relevance t
                         :documentation "A dictionary of named constants referenced by the rate-fn property"))


(defield reaction-type.set-rate-function (fn &rest args)
  (funcall fn object args))

(defield reaction.rate ()
  (funcall .type.rate-fn object))

(defrule compute-reaction-rate-fn
  (?rxn [reaction ?rxn-type ?rxn-loc])
   =>
   (compute-reaction-rate-influences ?rxn ?rxn.reactants (lambda () {- ?rxn.rate}))
   (compute-reaction-rate-influences ?rxn ?rxn.products (lambda () ?rxn.rate)))

(defun compute-reaction-rate-influences (rxn req-rct-pairs ratefn)
  (mapc (lambda (assoc) 
          (let* ((r-req         (car assoc))
                 (rct           (cdr assoc))
                 (influence     (lambda () {r-req.stoichiometry * (funcall ratefn)
                                                                * rxn.location.size})))
            (declare (type reaction-type-requirement r-req)
                     (type species rct))
            {rct.moles.(rate-of rxn r-req.side) := influence}))
        req-rct-pairs))

;;;
;;; species have moles, concentration
;;;
(defprop species.moles
    (:#= [[ode-var] :dimension *molecular-amount-dimension* 
          :min (quantity 0 *molecular-amount-dimension*)] :relevance t
     :documentation  "Time variant quantity representing moles of the species."))

;;;
;;; concentration

(defprop species.conc
    (:#= (let ((size    .location.size)
               (moles   .moles))
           [[derived-var moles {defer (expand {moles / size} 1)}]
            :inverse-fn {defer {object * size}.(expand 1)}])
     :relevance t
     :documentation "Time variant quantity representing concentration of the species."))
   
(defprop species-type.t0 ()) ; the time 0 concentration in moles or concentration for all species of this type

(defrule initialize-species-t0-variable
  (:and (?st [[species-type] :t0 ?t0])
        (?s [species ?st ?]))
  =>
  (let ((d ?t0.dimension))
    (cond
     ((and {d = *molecular-amount-dimension*}
           (not (eq *molecular-amount-dimension* null-dimension)))
      {?s.moles.t0 := ?t0})

     (t {?s.conc.t0 := ?t0}))))

