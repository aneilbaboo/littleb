;;;
;;; Demonstrates a complex location, "cell-location", and
;;; some reactions which occur within it.  We're using simple
;;; species-types here
;;;
(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) 

(def-location-class cell-location ()
  (cyto              compartment)
  (plasma-membrane   membrane :inner .cyto)
  (endosome          compartment)
  (endo-membrane     membrane :outer .cyto :inner .endosome)
  (inv-endo-membrane .membrane.inverse)
  (nucleus           compartment)
  (nuclear-membrane  membrane :outer .cyto :inner .nucleus))

;; define extra-cellular compartment
(define ecp [[compartment] {.size.value := 3}])

;; create an instance of a cell location:
(define cell [[cell-location] 
              {.plasma-membrane.outer := ecp}
              {.cyto.size.value := 2}
              {.plasma-membrane.size.value := 2}
              {.endo-membrane.size.value := .1}
              {.endosome.size.value := .1}
              {.nucleus.size.value := .3}
              {.nuclear-membrane.size.value := .4}])

(def-species-type membrane channel)
(def-species-type compartment ion)

;; channel allows membrane to go through membrane 
{ion @ :outer + channel <-> ion @ :inner}

;; ion goes through membrane without needing channel
;; note how we must refer explicitly to the location class:
{ion @ membrane.outer <-> ion @ membrane.inner}

;; channel is translocated from endosome to plasma-membrane:
{channel @ cell-location.endosome -> channel @ cell-location.plasma-membrane}

