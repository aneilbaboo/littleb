(in-package b-user)

(include (@folder/egfr-mapk ;; include the egfr-mapk base model
          @folder/reactions/mkp-mapk-cascade-k1)) ;; add the map kinase phosphatase neg feedback

cell.inner.(contains mkp-gene)

{mkp-gene.t0 := .1 millimoles}

