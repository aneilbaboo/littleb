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

;;; $Id: ode.lisp,v 1.13 2008/01/22 16:42:40 amallavarapu Exp $

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
(defprop reaction-type.rate-fn (:documentation "A math expression describing rate in terms of the species-types and localizations which make up the left hand side"))

(defprop reaction-type.rate-calculator
    (:documentation "A list which stores the user's original rate specification (the arguments to set-rate-function)"))

(defprop reaction-type.k 
    (dictionary :#= [dictionary] :relevance t
                :documentation "A dictionary of named constants referenced by the rate-fn property"))

(defield reaction-type.set-rate-function (fn &rest args)
  (let ((rate-dimension {(location-class-dimension .location-class)
                         * *molecular-amount-dimension* 
                         / *time-dimension*}))
    (setf .rate-calculator (list* fn args)
          .rate-fn         (apply
                            fn
                            args
                            rate-dimension 
                            .k
                            (apply #'mapcar #'list
                                   (mapcar (lambda (rtr)
                                             (list rtr.(localization t)
                                                   rtr.stoichiometry
                                                   (location-class-dimension
                                                    rtr.species-type.location-class)))
                                           .lhs-requirements))))))

;;; DEFINE-CUSTOM-RATE: allows user to define a rate calculator 
;;;     - function of 5 or more arguments which return a math expression expression
;;;     - may also set entries in a dictionary (usually constants)
;;;
;;; INPUTS: LAMBDA-LIST     - user-supplied arguments to .SET-RATE-FUNCTION
;;;      
;;;         ENTITIES        - list of n entities (e.g., species-type objects)
;;;         STOICHIOMETRIES - list of n integers representing stoichiometries of the entities
;;;         DIMENSIONS      - list of n dimensions representing the concentration dimension of each entity
;;;         RATE-DIMENSION  - the dimension of the expression to be calculated 
;;;                           (the implementation may use this value to fix user inputs,
;;;                           produce warnings or errors)
;;;         DICTIONARY      - a DICTIONARY object which provides a namespace for any new objects
;;;                           which must be generated (e.g., new reference variables)
;;;
;;;
(define-macro define-custom-rate 
    (name (&rest lambda-list)
          (&key rate-dimension entities stoichiometries dimensions)
          &body body)
  (let ((rate-dimension  (or rate-dimension '#:rate-dimension))
        (dictionary      '#:dictionary)
        (entities        (or entities '#:entities))
        (stoichiometries (or stoichiometries '#:stoichiometries))
        (dimensions      (or dimensions '#:dimensions))
        (param-name      '#:param-name)
        (param-value     '#:param-value))
    (mutils:let+ 
        (((user-doc body) (if (stringp (first body)) (values (first body) (rest body))
                            (values nil body)))
         (doc-str         (format nil "USAGE: x.(set-rate-function '~S ,@lambda-list).~@[  ~A~]"
                                  name user-doc))
         (user-args       '#:user-args))
      `(progn
         (defmethod documentation ((o (eql ',name)) (doc-type (eql 'function)))
           ,doc-str)
         (define-function ,name 
             (,user-args ,rate-dimension ,dictionary ,entities ,stoichiometries ,dimensions)
           (declare (ignorable ,rate-dimension ,dictionary ,entities
                               ,stoichiometries ,dimensions))
           (flet ((,(intern "STORE-PARAMETER") (,param-name ,param-value)
                    {,.dictionary.,param-name :#= (ensure-reference-var ,param-value)}
                    ,param-name))
             (destructuring-bind ,lambda-list ,user-args
               ,@body)))))))

(defprop reaction.%precomputed-rate (:= nil))

(defrule reset-reaction-precomputed-rate
  "When .rate-fn of reaction-type changes, delete the precomputed rate of the reaction"
  (?rxn [reaction (?type [[reaction-type] :rate-fn ?rf])])
  =>
  (setf ?rxn.%precomputed-rate nil))

(defield reaction.rate ()
  (or .%precomputed-rate
      (setf .%precomputed-rate
            .type.rate-fn.(map-substitution (reaction-rate-substituter object)))))

(deftype species-type-localization ()
  `(and localization
        (satisfies localization-entity-is-species-type-p)))

(defun localization-entity-is-species-type-p (x)
  (species-type-p x.entity))

(defun reaction-rate-substituter (rxn)
  (let ((substs (|REACTION.SUBSTITUTION-TABLE| rxn))
        (type-substs rxn.type.k._hash-table))
    (labels ((substitute (o)
               (typecase o
                 (function     (funcall o rxn))
;;;;                  ((or species-type
;;;;                       species-type-localization)
;;;;                   (let ((species (gethash o substs)))
;;;;                     (unless species (b-error "BUG: Unable to substitute ~S; ~
;;;;                                                  no species exists for this species-type."
;;;;                                              o))
;;;;                     species.conc))
                 (species-type      rxn.type.(lhs-species rxn.location o))
                 (localization      (list)
                                    rxn.type.(lhs-species 
                                              rxn.location
                                              (or (gethash o type-substs) 
                                                  (b-error "BUG IN REACTION-RATE-SUBSTITUTER - CANNOT SUBSTITUTE ~S" o))))
                 (t            (let ((subst (or (gethash o type-substs)
                                                (gethash o substs))))
                                 (if subst (substitute subst)
                                   o))))))
      #'substitute)))

(defrule add-reaction-influences
  (?rxn reaction); ?rxn-type ?rxn-loc])
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

