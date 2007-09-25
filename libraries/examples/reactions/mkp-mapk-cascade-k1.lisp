(in-package b-user)

(include (@folder/mkp-mapk-cascade
          b/units/standard
          b/biochem/std-rate-functions))

mkp-mapkkk-dephosphorylation.(set-rate-function 'mass-action {.0001 / molar / seconds})
mkp-mapkk-dephosphorylation.(set-rate-function 'mass-action {.00005 / molar / seconds})
mkp-mapk-dephosphorylation.(set-rate-function 'mass-action {.00001 / molar / seconds})
