(in-package b-user)

;;;
;;; Describes the simple enzymatic reaction:       Enz
;;;                                             Sub --> Prod
;;;
;;; Showing how to generate single or multi-step reactions:
;;;

(include (b-user/non-dimensional-ode-biochemistry
          b/matlab/ode-translation))

;; SPECIES
(def-species-types compartment enz sub prod)

;; REACTIONS

;; option1 single step reversible reaction:
;;;; (define enzrxn [[enzymatic-reaction {enz} {sub} {prod}]
;;;;                 :steps '(:reversible)  ;; set the .steps property, 
;;;;                                        ;; which creates enzrxn.fwd.1 and rev.1
;;;;                 (.set-rate-function    ;; set the rate function 
;;;;                  'mass-action          ;;           
;;;;                  :fwd '( 2 4)             ;; for the fwd.1 
;;;;                  :rev '(2 2))])          ;; and the rev.1 steps

;; option2 two step reaction, where second step is irreversible:
;;         a species representing the enzyme-substrate complex, enzrxn.es.1, 
;;         is generated on your behalf:
(define enzrxn [[enzymatic-reaction enz sub prod]
                :steps '(:reversible :irreversible)  ;; set the .steps property
                (.set-rate-function                  ;; set the rate function
                 'mass-action                     
                 :fwd '(4 2)           ;; for the fwd.1 and fwd.2 steps
                 :rev '(2))])          ;; and the rev.1 step (there is not rev.2 step)

;; LOCATION
(define c1 [compartment])

;; MODEL
c1.(contains enz sub)

{enz.t0 := .1}
{sub.t0 := .2}
{prod.t0 := 0}

(create-ode-model "enzrxn")
