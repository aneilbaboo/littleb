(in-package :b-user)

(include b-user/ode-biochem)

(def-species-types compartment e s p)

;; hill function, using a one-off custom rate function:
{e + s -> p}.
  (set-rate-function 'custom-rate {s ^ :hill / {s ^ :hill + :half-max ^ :hill}}
                     :hill 1.5
                     :half-max .5)

;; alternatively, provide a reusable custom rate-function

;; Note: the following code is just an example - you should use the HILL
;; function provided in b/biochem/std-rate-functions 
(define-custom-rate hill-function (s &key (hill 1) (half-max 1))
    () ; not used in this example - take a look at b/biochem/std-rate-functions for more info

  ;; these two lines store the values hill and half-max in a dictionary named K
  ;; inside the reaction-type, using the keys :hill and :half-max.
  ;; the resulting reaction type will have values accessible using
  ;; the-rxn.k.hill and the-rxn.k.half-max
  (save-parameter :hill hill)
  (save-parameter :half-max half-max)

  {s ^ :hill / {s ^ :hill + :half-max ^ :hill}})

;; then, we can simply say:
{e + s -> p}.(set-rate-function 'hill s :hill 1.5 :half-max .5)


               