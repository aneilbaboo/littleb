(in-package b-user)

;;;
;;; Describes a simple reaction:       G6PI
;;;                                 G6P --> F6P
;;;
;;;     which is interpreted as a mass-action reaction which 
;;;           involves a single enzyme-substrate complex
(include (b-user/ode-biochem
          b/matlab/ode-translation))

;; SPECIES
(define g6pi [[species-type] :documentation "glucose-6-phospho-isomerase"])
(define g6p [[species-type] :documentation "glucose-6-phosphate"])
(define f6p [[species-type] :documentation "fructose-6-phosphate"])

;; REACTIONS
(define g6pirxn [[enzymatic-reaction {g6pi} {g6p} {f6p}]
                 :steps '(:reversible :reversible)
                 (.set-rate-function 
                  'mass-action 
                  :fwd '(4 2)
                  :rev '(2 2))])

(define g6pi-x [[species-type] :documentation "inhibited g6pi"])
(define inhib-fwd [reaction-type {g6pi + f6p} {g6pi-x}])
(define inhib-rev [reaction-type {g6pi-x} {g6pi + f6p}])
inhib-fwd.(set-rate-function 'mass-action 100)
inhib-rev.(set-rate-function 'mass-action 100)
;; LOCATION
(define c1 [compartment])

c1.(contains g6pi)
c1.(contains g6p)

{g6pi.t0 := .1}
{g6p.t0 := .2}
{f6p.t0 := 0}

(create-ode-model "g6pi")
