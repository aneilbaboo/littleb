(in-package b-user)

(include (b-user/3d-ode-biochem
          b/matlab/ode-translation) :expose)

;;;  SPECIES
(define g6pi [[species-type] :documentation "glucose-6-phospho-isomerase"])

(define g6p [[species-type] :documentation "glucose-6-phosphate"])

(define f6p [[species-type] :documentation "fructose-6-phosphate"])


;;; REACTIONS
(define g6pi-rxn-fwd [reaction-type {g6pi + g6p} {g6pi + f6p}])
(define g6pi-rxn-rev [reaction-type {g6pi + f6p} {g6pi + g6p}])


;;; ASSUMPTIONS
g6pi-rxn-fwd.(set-rate-function 'mass-action {2 / molar / seconds})
g6pi-rxn-rev.(set-rate-function 'mass-action {.1 / molar / seconds})


;;; GLOBAL STRUCTURE
(define c1 [[compartment] {.size.value := {40 micrometers} ^ 3}])

(defun create-g6pi-model (&key (g6pit0 {1 micromolar})
                               (g6pt0 {200 millimolar})
                               (f6pt0 {0 millimolar}))
  c1.(contains g6pi)
  c1.(contains g6p)
  
  {g6pi.t0 := g6pit0}
  {g6p.t0 := g6pt0}
  {f6p.t0 := f6pt0}
  (create-ode-model "G6PI")
  )

(format t "~&; Try entering these lines at the listener prompt:~
           ~&; B-USER >  c1.(contains g6pi)~
           ~&; B-USER >  c1.(contains g6p)~
           ~&; B-USER >  {g6pi.t0 := 1 micromolar}~
           ~&; B-USER >  {g6p.t0 := 200 millimolar}~
           ~&; B-USER >  {f6p.t0 := 0 millimolar}~
           ~&; B-USER >  (create-ode-model  ~S)~
           ~&; or try this:~
           ~&; B-USER > (create-g6pi-model)" #1I@FILE)
