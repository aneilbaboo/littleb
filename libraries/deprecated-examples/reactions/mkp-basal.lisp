(in-package b-user)

(include b/biochem)

(define mkp-production [reaction-type {mapk* + mkp-gene} {mapk* + mkp} compartment])
(define mkp-degradation [reaction-type {mkp} {} compartment])
