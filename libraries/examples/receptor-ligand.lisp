;;; Defines a receptor, a ligand, and an internal 
;;; signal transduction molecule.

;;; The receptor dimerizes when bound to ligand
;;; and phosphorylates itself

(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) 

;;; define the monomers
(defmonomer (receptor membrane)
  "Receptor molecule" ;; this documentation string is optional
  (L :documentation "ligand binding site")
  (P :documentation "phosphorylation site"
     :states (member :u :p)) ; the :STATES keyword makes this a state site
  (D :documentation "dimerization site")
  (C :documentation "internal binding site"))

(defmonomer ligand
  "the ligand"
  (R :documentation "receptor binding site"))

(defmonomer sigtrans
  "Signal transduction molecule" 
  (C :documentation "binds to receptor C site"))
 
;; receptor-ligand binding: R + L <-> L-R
;; double arrows <<->> are required when patterns containing * are used:
{[receptor L._ **] + [ligand _] @ :outer <<->> [[receptor L.1][ligand 1]]}. ; don't forget the DOT
 (set-rate-function 'mass-action :fwd 1 :rev 1)

;; receptor-dimerization: L-R + R <-> L-R-R
{[[receptor L.1 D._ **][ligand 1]] + [receptor D._ **] 
 <<->> 
 [[receptor L.1 D.2 **][ligand 1][receptor D.2 **]]}.
 (set-rate-function 'mass-action :fwd 1 :rev 1)

;; receptor cross-phosphorylation: Ru-R* <-> Rp-R*
{[[receptor D.1 P.U **][receptor D.1 **]] ->>
 [[receptor D.1 P.P **][receptor D.1 **]]}.
 (set-rate-function 'mass-action 1)

;; signal transduction molecule binding: Rp + S <-> S-Rp
{[receptor P.P C._ **] + [sigtrans C._] @ :inner <<->>
 [[receptor P.P C.1 **][sigtrans C.1]]}.
 (set-rate-function 'mass-action :fwd 1 :rev 1)
 
;; define compartments:
(define cyto [[compartment] {.size.value := 1}
              :documentation "cytoplasm"])
(define ecm [[compartment] {.size.value := 10} 
             :documentation "Extra-cellular mileu"])
(define m [[membrane] {.size.value := .5}
              :inner cyto :outer ecm
              :documentation "membrane"])

;; put molecules in locations:
m.(contains [receptor]) ; [receptor] = the default receptor [receptor L._ P.U D._ C._]
ecm.(contains [ligand])
cyto.(contains [sigtrans])

;; set initial conditions:
{[receptor].(in m).conc.t0 := .5}
{[sigtrans].(in cyto).conc.t0 := .5}
{[ligand].(in ecm).conc.t0 := 10}

;; create a matlab model
(create-ode-model "rls" :vars (query species.conc).alpha-order) ; variables will be alphabetically ordered

