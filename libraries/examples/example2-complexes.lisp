(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) ; or (include b-user/3d-ode-biochem)

;;; define the monomers
(defmonomer mol1 a)
(defmonomer mol2 b)
 
;; define the reactions
{[mol1 _] + [mol2 _] 
 ->> 
 [[mol1 1][mol2 1]]}.
 (set-rate-function 'mass-action 2)
 ;; etc.

;; define compartments:
(define c1 [[compartment] {.size.value := 2}])

;; put stuff in compartments:
c1.(contains [mol1] [mol2])

;; set initial conditions:
{[mol1].(in c1).conc.t0 := .5}
{[mol2].(in c1).conc.t0 := 1}

;; create a matlab model
(create-ode-model "mol1_2" :vars (query species.conc).alpha-order) ; variables will be alphabetically ordered

