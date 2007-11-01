(in-package :b-user)

(include b-user/ode-biochem)


;;;
;;; DEFINE MONOMERS:
;;; 
(defmonomer (egfr membrane) 
  "The EGF Receptor: one external ligand binding domain, an internal protein-binding domain and a phosphorylation site"
  L SHC (P :states (member :u :p)))

(defmonomer egf 
  "The epidermal growth factor - has a single site for binding the receptor"
  R)

(defmonomer shc
  SHC)


;;;
;;; DEFINE COMPLEX-REACTION-TYPES:
;;;
;; EGFR and EGF bind via the L and R sites:
(define egf-binding {[egfr _ * *] + [egf _] @ :outer ;; egf must be in the :C1 compartment of the membrane
                     ->> [[egfr 1][egf 1]]})

;; SHC binds to EGFR when located in the C2 membrane
(define shc-egfr-binding {[egfr * _ *] + [shc] @ :outer ;; Note: [shc] is shorthand for [shc _]
                          ->>
                          [[egfr * 1 *][shc 1]]})


;; set the rate functions:
egf-binding.(set-rate-function 'mass-action 2)
shc-egfr-binding.(set-rate-function 'mass-action 2)


;;;
;;; ADD TO A COMPARTMENT:
;;;
(define dish [compartment])
(define sc [[spherical-cell] :outer dish])

sc.membrane.(contains [egfr])
sc.outer.(contains [egf])
sc.inner.(contains
