(in-package b-user)

(include (b/biochem
          @library/species/egfr))

(define egfr-egf-binding [reaction-type {egfr + egf.(required :c1)} 
                                        {egfr+egf}
                                        membrane])

(define egfr-egf-unbinding [reaction-type {egfr+egf} 
                                          {egfr + egf.(required :c1)} 
                                          membrane])

