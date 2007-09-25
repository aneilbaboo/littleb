(in-package b-user)

(include (b-user/non-dimensional-ode-biochemistry
          b/matlab/ode-translation) :expose)

;; SPECIES
(define g6pi [[species-type] :documentation "glucose-6-phospho-isomerase"])

(define g6p [[species-type] :documentation "glucose-6-phosphate"])

(define f6p [[species-type] :documentation "fructose-6-phosphate"])

(define g6pi-rxn-es [[species-type] :documentation "enzyme complex of g6pi rxn"])

;;; REACTIONS
(define g6pi-rxn-fwd1 [reaction-type {g6pi + g6p} {g6pi-rxn-es}])
(define g6pi-rxn-rev1 [reaction-type {g6pi-rxn-es} {g6pi + g6p}])
(define g6pi-rxn-fwd2 [reaction-type {g6pi-rxn-es} {g6pi + f6p}])
(define g6pi-rxn-rev2 [reaction-type {g6pi + f6p} {g6pi-rxn-es}])

;;; KINETICS
g6pi-rxn-fwd1.(set-rate-function 'mass-action 1)
g6pi-rxn-rev1.(set-rate-function 'mass-action 3)
g6pi-rxn-fwd2.(set-rate-function 'mass-action 1)
g6pi-rxn-rev2.(set-rate-function 'mass-action .00001)


;; LOCATIONS
(define c1 [[compartment] {.size.value := 1}])

(defun create-g6pi-model (&key (g6pit0 .000001)
                               (g6pt0 .2)
                               (f6pt0 0))
  c1.(contains g6pi)
  c1.(contains g6p)
  
  {g6pi.t0 := g6pit0}
  {g6p.t0 := g6pt0}
  {f6p.t0 := f6pt0}
  (create-ode-model  "G6PI")
  )


(format t "(INIT-G6PI-MODEL) to initialize this model.~%~
           (create-ode-model  \"g6pi\") to generate MATLAB scripts.~%"
        (package-name *package*))
