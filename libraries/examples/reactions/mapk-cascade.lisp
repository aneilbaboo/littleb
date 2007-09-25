(in-package b-user)

(include (@library/species/egfr 
          @library/species/mapk-cascade
          b/biochem/enzymatic-reaction))

(define egfr-mapkkk-activation [enzymatic-reaction {egfr+egf} 
                                                   {mapkkk.(required :c2)} 
                                                   {mapkkk*.(required :c2)} 
                                                   '(:reversible :reversible)
                                                   membrane])

(define mapkk-activation [enzymatic-reaction {mapkkk*} 
                                             {mapkk} 
                                             {mapkk*} 
                                             '(:reversible :reversible)
                                             compartment])

(define mapk-activation [enzymatic-reaction {mapkk*} 
                                            {mapk} 
                                            {mapk*} 
                                            '(:reversible :reversible)
                                            compartment])
