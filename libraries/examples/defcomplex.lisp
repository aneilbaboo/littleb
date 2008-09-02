;;;
;;; DEFCOMPLEX - expands into a set of monomers
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

(defmonomer mon dim a b (p :states (member :u :p))

;;; (DEFCOMPLEX name sites complex-definition)
(defcomplex dimer (b1 b2 p1.u p2.u) ;; note: state must be provided for state sites
  `[[mon a.!1 b1.,b1 p1.,p1]        ;; use ! for bonds in complex definitions
    [mon a.!1 b2.,b2 p2.,p2]])

;;; examples

[dimer] ;; == [[mon b._ a.1 p.u][mon a.1 b._ p.u]]

[dimer p1.p] ;; == [[mon a.1 b._ p.p][mon a.1 b._ p.u]]

[[dimer b1.1 p1.p][dimer b2.1 p2.p]] ;; == [[mon a.2 b.1 p.p][mon a.2 b._ p.u]
                                     ;;     [mon a.3 b._ p.p][mon a.3 b.1 p.p]]