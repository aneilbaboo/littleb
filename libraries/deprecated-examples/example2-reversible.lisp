;;;
;;; This file demonstrates how to define a reversible reaction.
;;; This defines an equivalent model to example1.lisp
;;;   
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

;; SPECIES
(def-species-types compartment enz sub prod)

;; REACTIONS
(define rxn {enz + sub <-> enz + prod})  ;; generates a reversible-reaction object
                                         ;; which contains {enz + sub -> enz + prod} in the .fwd property
                                         ;; and      {enz + prod -> enz + sub} in the .rev property

rxn.fwd.(set-rate-function 'mass-action 1)  ;; note how we access the forward
rxn.rev.(set-rate-function 'mass-action .5) ;; and reverse reaction components

;;; LOCATIONS
(define c1 [compartment]) ; by default, {.size.value := 1}

;; MODEL
c1.(contains enz sub) ;; put enz and sub in the compartment - prod.(in c1) is inferred

 
{enz.(in c1).conc.t0 := .1}
{sub.(in c1).conc.t0 := .2}
{prod.(in c1).conc.t0 := 0} ; not necessary, since t0 = 0 by default

(create-ode-model  "esprev")
