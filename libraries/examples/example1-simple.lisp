;;;
;;; Demonstrates a very simple symbol-based approach to writing models
;;; using the DEF-SPECIES-TYPE macro.  Species-types have no structure.
;;; This is an alternate approach - Cf. DEFMONOMER examples elsewhere
;;; in this library.
;;;
(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) ; or (include b-user/3d-ode-biochem)

;;; define the species-types
(def-species-types compartment 
  (e  :documentation "the enzyme")
  (s  :documentation "the substrate")
  (es :documentation "the enzyme-substrate product")
  (p  :documentation "the product"))

; shorthand: (def-species-type compartment e s es p) 

;; define the reactions

;; note the :fwd/:rev keywords for reversible <-> reaction:
{e + s <-> es}.(set-rate-function 'mass-action :fwd .1 :rev 1)

;; no :fwd/:rev keywords needed for reversible reaction:
{es -> p}.(set-rate-function 'mass-action .5)

;; define compartments:
(define c1 [[compartment] {.size.value := 1}])

;; put species-types in compartments - this creates species objects
c1.(contains e s)

;; species objects are referred to using stype.(in location) syntax:
;; set initial conditions:
{e.(in c1).conc.t0 := .5}
{s.(in c1).conc.t0 := 1}

;; create a matlab model based on dmoles/dt
(create-ode-model "esp") ;; writes several files to the folder containing this file

;; use
;; (create-ode-model "esp" :vars (query species.conc.alpha-order))
;; to create a concentration-based model



