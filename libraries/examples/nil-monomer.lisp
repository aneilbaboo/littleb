
;; The NIL monomer is used to stand in for a monomer
;; which should be ignored.  It is primarily useful 
;; in substitution-tables and data-tables, where a
;; you want to encode a set of related complexes one
;; of which may differ by excluding a member of the 
;; complex.

(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) 


;; three monomers  
(defmonomer mon1 A D)
(defmonomer mon2 A D)  
(defmonomer mon3 A D)

;; activator
(defmonomer act e)

;; the monomers dimerize when bound to an activators
;; except for mon3 which can dimerize independently of Act2
(with-substitution-table 
    (($M1     $M2      $A   $K)
     (mon1    mon1     act  .1)
     (mon1    mon2     act  .2)
     (mon2    mon1     act  .5) ; mon2-act binds mon1 
     (mon2    mon1     nil  .3)
     (mon2    mon2     nil  .5)) ; unbound mon2 binds unbound mon1
  {[[$M1 1 _][$A 1]] + [$M2 * _]
   ->>
   [[$M1 1 2][$A 1][$M2 * 2]]}.
   (set-rate-function 'mass-action $K))
     
