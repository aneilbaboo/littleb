;;;
;;; This file demonstrates the low-level syntax 
;;; for specifying the same system as example1.lisp
;;; 
(in-package :b-user)

(include b-user/ode-biochem)

;; SPECIES
(define enz [species-type])  ;; the same as this code in example1.lisp: 
(define sub [species-type])  ;; (def-species-types compartment enz sub prod)
(define prod [species-type]) ;;


;; REACTIONS 
(define fwd [reaction-type {enz + sub} {enz + prod}]) ;; example1.lisp: (define fwd {enz + sub -> enz + prod}) 
(define rev [reaction-type {enz + prod} {enz + sub}]) ;; example1.lisp: (define rev {enz + sub <- enz + prod}) 

fwd.(set-rate-function 'mass-action 1)
rev.(set-rate-function 'mass-action .5)

;; alternatively, one could write:
;; (define fwd [{enz + sub -> enz + prod} (.set-rate-function  'mass-action 1)])
;; (define rev [[reaction-type {enz + prod} {enz + sub}] (.set-rate-function  'mass-action .5)])

;;; LOCATIONS
(define c1 [compartment])

;; MODEL
c1.(contains enz sub)

;; alternatively, any one of the following lines achieves the same effect,
;;    since 
;; c1.(contains enz) c1.(contains sub) ; add enz and sub using separate calls to location.contain
;; c1.(contains enz prod) ; add the enzyme and product (reverse reaction creates the substrate)
;; c1.(contains fwd) ; add the fwd reaction, left and right hand species are created automatically, 
;;                   ;  and the rev reaction is inferred
;; c1.(contains rev) ; add the reverse reaction
 
{enz.(in c1).conc.t0 := .1} 
{sub.(in c1).conc.t0 := .2}
{prod.(in c1).conc.t0 := 0} ; not necessary, since t0 = 0 by default

;; a shortcut which sets the t0 concentration of EVERY enz species (in all locations) is:
;; {enz.t0 := 0}

(create-ode-model  "ESP")
