(in-package b-user)

(include b/biochem :use)

(define egfr [[species-type] :location-class membrane
             :documentation"a transmembrane receptor"])

(define egf [[species-type] :location-class compartment
                :documentation "a egf which binds to egfr"])

(define egfr+egf [[species-type] :location-class membrane
                    :documentation "egf bound to egfr"])


