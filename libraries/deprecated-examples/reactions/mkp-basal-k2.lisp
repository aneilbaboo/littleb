(in-package b-user)

(include (@folder/mkp-basal 
          b/units/standard
          b/biochem/std-rate-functions))

mkp-production.(set-rate-function 'mass-action {.0000001 / molar / seconds})
mkp-degradation.(set-rate-function 'mass-action {.001 / seconds})
