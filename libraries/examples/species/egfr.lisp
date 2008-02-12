(in-package b-user)

(include b/biochem :use)

(def-species-types membrane
  (egfr :documentation "a transmembrane receptor")
  (egfr+egf :documentation "egf bound to egfr"))

(def-species-types compartment
  (egf :documentation "a ligand which binds to egfr"))



