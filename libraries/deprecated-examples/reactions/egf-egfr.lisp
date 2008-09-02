(in-package b-user)

(include (b/biochem
          @library/species/egfr))

(define egfr-egf-binding {egfr + egf.(required :c1) ->
                               egfr+egf})

(define egfr-egf-unbinding {egfr+egf -> egfr + egf.(required :c1)} )

