;;;
;;; demonstrates the manual approach to defining an enzyme substrate
;;;   reaction involving an enzyme substrate complex:
;;;
;;;    e + s <-> es <-> e + p
;;;
;;;
(in-package b-user)

(include (b-user/ode-biochem
          b/matlab/ode-translation) :expose)

;; SPECIES
(def-species-types compartment 
  (enz :documentation "the enzyme")
  (sub :documentation "the substrate")
  (f6p :documentation "the product")
  (enz-rxn-es :documentation "enzyme substrate complex"))

;;; REACTIONS
(define enz-rxn-step1 {enz + sub <-> enz-rxn-es})
(define enz-rxn-step2 {enz-rxn-es <-> enz + f6p})

;;; KINETICS
enz-rxn-step1.fwd.(set-rate-function 'mass-action 1)
enz-rxn-step1.rev.(set-rate-function 'mass-action 3)
enz-rxn-step2.fwd.(set-rate-function 'mass-action 1)
enz-rxn-step2.rev.(set-rate-function 'mass-action .00001)


;; LOCATIONS
(define c1 [[compartment] {.size.value := 1}])

;; MODEL:
c1.(contains enz sub)
  
{enz.t0 := .000001}
{sub.t0 := .2}
{f6p.t0 := 0}
(create-ode-model  "example3")

