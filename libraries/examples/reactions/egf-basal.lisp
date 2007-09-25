(in-package b-user)

(include (b/biochem 
          @library/species/egfr))

(define egf-degradation [reaction-type {egf} {} compartment])
