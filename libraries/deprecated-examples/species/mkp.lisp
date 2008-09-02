(in-package b-user)

(include b/biochem :use)

(def-species-types compartment
  (mkp :documentation "mapkinase phosphatase")
  (mkp-gene :documentation "mapkinase phosphatase gene"))(in-package b-user)

(include (b/biochem 
          @library/species/egfr))

(define egf-degradation {egf -> })

