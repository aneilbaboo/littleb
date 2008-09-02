(in-package b-user)

(include (@library/species/mapk-cascade
          @library/species/mkp
          b/biochem))

(define mkp-mapkkk-dephosphorylation [reaction-type {mapkkk* + mkp} {mapkkk + mkp} compartment])
(define mkp-mapkk-dephosphorylation [reaction-type {mapkk* + mkp} {mapkk + mkp} compartment])
(define mkp-mapk-dephosphorylation [reaction-type {mapk* + mkp} {mapk + mkp} compartment])
