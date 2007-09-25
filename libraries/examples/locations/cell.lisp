(in-package b-user)

(include b-user/3d-ode-biochemistry)

(define dish [[compartment] :size {2 milliliters}])
(define cell [[membrane-enclosure]
              {.membrane.size :#= [[reference-var] :value {4  pi {30 micrometers} ^ 2}]}
              {.inner.size :#= [[reference-var] :value {4/3 pi {30 micrometers} ^ 3}]}
              {.membrane.c1 := .outer := dish}])
              
