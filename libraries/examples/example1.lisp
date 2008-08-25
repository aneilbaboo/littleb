;;;
;;; This file demonstrates how to define a simple reversible enzyme-catalyzed reaction, 
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

;; SPECIES
(def-species-types compartment
  (e  :documentation "enzyme")
  (s  :documentation "substrate")
  (es :documentation "enzyme-substrate complex")
  (p  :documentation "product"))

;; REACTIONS
{e + s <-> es}.(set-rate-function 'mass-action :fwd 1 :rev .4)
{es -> p}.(set-rate-function 'mass-action .1)

;;; LOCATIONS
(define c1 [compartment]) ; by default, {.size.value := 1}

;; MODEL
c1.(contains e s) ;; put enz and sub in the compartment - p.(in c1) is inferred

 
{e.(in c1).conc.t0 := .1}
{s.(in c1).conc.t0 := .2}
{p.(in c1).conc.t0 := 0} ; not necessary, since t0 = 0 by default

(create-ode-model  "ESP")
