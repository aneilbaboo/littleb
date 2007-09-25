;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2007 Aneil Mallavarapu

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.

;;; File: b-standard_dimensions
;;; Description: Defines some standard base and derived dimensions.

;;; $Id: standard-dimensions.lisp,v 1.1 2007/09/25 17:54:06 amallavarapu Exp $
;;;

(in-package #I@FILE)
(include (@FOLDER/dimensionalization 
          @FOLDER/unit-systems 
          @LIBRARY/math ) :use) 

(#-:allegro progn #+:allegro excl:without-package-locks
(define time [[base-dimension] :unit-system SI-UNITS]))

(define distance [[base-dimension] :unit-system SI-UNITS])
(define mass [[base-dimension] :unit-system SI-UNITS])
(define molecular-amount [[base-dimension] :unit-system SI-UNITS])
(define current [[base-dimension] :unit-system SI-UNITS])
(define temperature [[base-dimension] :unit-system SI-UNITS])
(define luminous-intensity [[base-dimension] :unit-system SI-UNITS])

(assume :dimensionalization 
  {*time-dimension* := time}
  {*distance-dimension* := distance}
  {*mass-dimension* := mass}
  {*molecular-amount-dimension* := molecular-amount}
  {*current-dimension* := current}
  {*temperature-dimension* := temperature}
  {*luminous-intensity-dimension* := luminous-intensity})

(define area [{distance ^ 2} :unit-system SI-UNITS])
(define volume [{distance ^ 3} :unit-system SI-UNITS])
(define concentration [{molecular-amount / volume} :unit-system SI-UNITS])

(define velocity [{distance / time} :unit-system SI-UNITS])
(define acceleration [{velocity / time}  :unit-system SI-UNITS])
(define momentum [{mass velocity} :unit-system si-units])
(define force [{mass * acceleration} :unit-system SI-UNITS])
(define energy [{force * distance} :unit-system SI-UNITS])
(define charge [{current * time} :unit-system SI-UNITS])
(define pressure [{force / area} :unit-system SI-UNITS])
(define power [{energy / time} :unit-system SI-UNITS])
(define electric-potential [{energy / charge} :unit-system SI-UNITS])
(define capacitance [{charge / electric-potential} :unit-system SI-UNITS])
(define conductance [{current / electric-potential} :unit-system SI-UNITS])
(define luminance [{luminous-intensity / area} :unit-system SI-UNITS])

(define density [{mass / volume} :unit-system SI-UNITS])
(define coercivity [{current / distance} :unit-system SI-UNITS])
(define resistance [{electric-potential / current} :unit-system SI-UNITS])
(define inductance [{mass ^ 2 area / { time ^ 2 current ^ 2}} :unit-system SI-UNITS])
(define magnetic-flux [{electric-potential time} :unit-system SI-UNITS])
(define magnetic-flux-density [{magnetic-flux / area} :unit-system SI-UNITS])
(define wave-number [{distance ^ -1} :unit-system SI-UNITS])
(define specific-volume [{volume / mass} :unit-system SI-UNITS])
(define current-density [{current / area} :unit-system SI-UNITS])
(define magnetic-field-strength [{current / distance} :unit-system SI-UNITS])
(define frequency [{time ^ -1} :unit-system SI-UNITS])
(define dynamic-viscosity [{pressure / time} :unit-system SI-UNITS])
(define surface-tension [{force / distance} :unit-system SI-UNITS])
(define irradiance [{power / area} :unit-system SI-UNITS])
(define heat-capacity [{energy / temperature} :unit-system SI-UNITS])
(define specific-heat [{energy / {temperature mass}} :unit-system SI-UNITS])
(define specific-energy [{energy / mass} :unit-system SI-UNITS])
(define thermal-conductivity [{power / {distance temperature}} :unit-system SI-UNITS])
;(define energy-density [{energy / volume} :unit-system SI-UNITS]) same as pressure
(define electric-field-strength [{electric-potential / distance} :unit-system SI-UNITS])
(define electric-charge-density [{charge / volume} :unit-system SI-UNITS])
(define electric-flux-density [{charge / area} :unit-system SI-UNITS])
(define permittivity [{capacitance / distance} :unit-system SI-UNITS])
(define molar-energy [{energy / molecular-amount} :unit-system SI-UNITS])
(define molar-entropy [{energy / {molecular-amount temperature}} :unit-system SI-UNITS])
(define exposure [{charge / mass} :unit-system SI-UNITS])
(define catalytic-activity [{molecular-amount / time} :unit-system SI-UNITS])
(define catalytic-concentration [{catalytic-activity / volume} :unit-system SI-UNITS])
(define molecular-weight [{mass / molecular-amount} :unit-system SI-UNITS])

(define UniMolar-Formation-Rate [{concentration / time} :unit-system b-units])
(define BiMolar-Formation-Rate [{concentration ^ 2 / time} :unit-system b-units])
(define UniMolar-Destruction-Rate [{{concentration time} ^ -1} :unit-system b-units])
(define BiMolar-Destruction-Rate [{{concentration ^ 2 time} ^ -1} :unit-system b-units])

(define volume^2 [{volume ^ 2} :unit-system SI-UNITS]) 
;;;
;;; Derived units which include angles:
;;;
;(define luminous-energy joule)
;(define luminous-flux {luminous-intensity solid-angle}) ; cannot, because solid-angle = null-dimension
;(define illuminance {luminous-flux / meter ^ 2} ; luminous-flux is undefined
;(define luminous-efficacy {luminous-flux / watt})
