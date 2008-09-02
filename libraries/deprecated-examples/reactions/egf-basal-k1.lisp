(in-package b-user)

(include (@folder/egf-basal 
          b/units/standard))

(include b/biochem/std-rate-functions)

egf-degradation.(set-rate-function 'mass-action {.05 / seconds})
    
