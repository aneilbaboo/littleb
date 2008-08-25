;;;
;;; This file demonstrates how to define a reversible reaction, 
;;;   
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

;; SPECIES
(def-species-types compartment enz sub prod)

;; REACTIONS
{enz + sub <-> enz + prod}.(set-rate-function 'mass-action :fwd 1 :rev .5)

;;; LOCATIONS
(define c1 [compartment]) ; by default, {.size.value := 1}

;; MODEL
c1.(contains enz sub) ;; put enz and sub in the compartment - prod.(in c1) is inferred

 
{enz.(in c1).conc.t0 := .1}
{sub.(in c1).conc.t0 := .2}
{prod.(in c1).conc.t0 := 0} ; not necessary, since t0 = 0 by default

(create-ode-model  "esprev")
