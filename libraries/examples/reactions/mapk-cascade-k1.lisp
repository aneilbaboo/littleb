(in-package b-user)

(include (@folder/mapk-cascade
          b/biochem/std-rate-functions))


egfr-mapkkk-activation.(set-rate-function 'mass-action
                                          :fwd (list {.0001 / molar / seconds} {1 / seconds})
                                          :rev (list {1000 / seconds}
                                                     {.0000000000000001 / millimolar / seconds}))
mapkk-activation.(set-rate-function 'mass-action                       
                                    :fwd (list {.00001 / molar / seconds} {1 / seconds})
                                    :rev (list {1000 / seconds}  {.0000000000000001 / millimolar / seconds}))

mapk-activation.(set-rate-function 'mass-action
                                   :fwd (list {.0000001 / molar / seconds} {1 / seconds})
                                   :rev (list {1000 / seconds}  {.0000000000000001 / millimolar / seconds}))
