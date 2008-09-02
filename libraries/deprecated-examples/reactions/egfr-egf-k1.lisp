(in-package b-user)

(include (@folder/egf-egfr 
          b/units/standard
          b/biochem/std-rate-functions))

egfr-egf-binding.(set-rate-function 'mass-action {10 / molar / seconds})
egfr-egf-unbinding.(set-rate-function 'mass-action {.05 / seconds})

