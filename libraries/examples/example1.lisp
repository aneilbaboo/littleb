;;;
;;; This file demonstrates how to define a simple reversible enzyme-catalyzed reaction, 
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

;; SPECIES
(def-species-types compartment enz sub prod)

;; REACTIONS
(define fwd {enz + sub -> enz + prod}) 
(define rev {enz + sub <- enz + prod}) 

fwd.(set-rate-function 'mass-action 1)
rev.(set-rate-function 'mass-action .5)

;;; LOCATIONS
(define c1 [compartment]) ; by default, {.size.value := 1}

;; MODEL
c1.(contains enz sub) ;; put enz and sub in the compartment - prod.(in c1) is inferred

 
{enz.(in c1).conc.t0 := .1}
{sub.(in c1).conc.t0 := .2}
{prod.(in c1).conc.t0 := 0} ; not necessary, since t0 = 0 by default

(create-ode-model  "ESP")
