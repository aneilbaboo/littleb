(in-package b-user)

(include (b-user/ode-biochem
          b/matlab/ode-translation))

;;;  SPECIES
(define g6pi [[species-type] :documentation "glucose-6-phospho-isomerase"])

(define g6p [[species-type] :documentation "glucose-6-phosphate"])

(define f6p [[species-type] :documentation "fructose-6-phosphate"])


;;; REACTIONS
(define g6pi-rxn-fwd {g6pi + g6p -> g6pi + f6p}) ; shorthand for [reaction-type {g6pi + g6p} {g6pi + f6p}])
(define g6pi-rxn-rev {g6pi + f6p -> g6pi + g6p}) ; shorthand for [reaction-type {g6pi + f6p} {g6pi + g6p}])


;;; ASSUMPTIONS
g6pi-rxn-fwd.(set-rate-function 'mass-action 8)
g6pi-rxn-rev.(set-rate-function 'mass-action 4)


;;; GLOBAL STRUCTURE
(define c1 [compartment]) ; by default, {.size.value := 1}

c1.(contains g6pi)
c1.(contains g6p)
 
{g6pi.t0 := .1}
{g6p.t0 := .2}
{f6p.t0 := 0}

(create-ode-model  "G6PI")
