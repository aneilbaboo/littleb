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

;;; File: b-standard_units
;;; Description: Defines standard units on top of the standard dimensions.

;;; $Id: standard-units.lisp,v 1.1 2007/09/25 17:54:06 amallavarapu Exp $
;;;

(in-package #I@FILE)

(include-declaration :use-packages (mallavar-utility))

(include (@FOLDER/unit-systems 
          @FOLDER/standard-dimensions 
          @LIBRARY/math 
          @FOLDER/dimensionalization) :use)

;; base units
(defmetricunit moles molecular-amount si-units)
(defmetricunit meters distance si-units :orders *all-orders-of-magnitude*)
(defmetricunit grams mass si-units :base-order :kilo)
(defmetricunit seconds time si-units)
(defmetricunit amperes current si-units)
(defmetricunit kelvin temperature si-units)
(defmetricunit candelas luminous-intensity si-units)

;; derived units
(defmetricunit coulombs {amperes * seconds} si-units)
(defmetricunit liters {{10 * centimeters} ^ 3} si-units)
(defmetricunit liters^2 {liters * liters} si-units) 
(defmetricunit molar {moles / liters} si-units)
(defmetricunit newtons {kilograms * meters / seconds ^ 2} si-units)
(defmetricunit joules {newtons * meters} si-units)
(defmetricunit hertz {seconds ^ -1} si-units)
(defmetricunit watts {joules / seconds} si-units)
(defmetricunit pascals {newtons / meters ^ 2} si-units)
(defmetricunit volts {joules / coulombs} si-units)
(defmetricunit farads {coulombs / volts} si-units)
(defmetricunit siemens {amperes / volts} si-units)
(defmetricunit webers {volts seconds} si-units)
(defmetricunit teslas {webers / meters ^ 2} si-units)
(defmetricunit henry {kilograms meters ^ 2 / {seconds ^ 2 amperes ^ 2}} si-units)
(defmetricunit ohms {volts / amperes} si-units)
(defmetricunit katals {moles / seconds} si-units)
(defmetricunit Pascals*Seconds {pascals seconds} si-units)
(defmetricunit Newtons/Meters {newtons / meters} si-units)
(defmetricunit Joules/Kelvin {joules / kelvin} si-units)
(defmetricunit Joules/Kilograms*Kelvin {joules / {kilograms kelvin}} si-units)
(defmetricunit Watts/Meters*Kelvin {watts / {meters kelvin}} si-units)
;;(defmetricunit joules/meters^3 {joules / meters ^ 3} si-units) - same as pascals
(defmetricunit volts/meters {volts / meters} si-units)
(defmetricunit coulombs/meters^3 {coulombs / meters ^ 3} si-units)
(defmetricunit coulombs/meters^2 {coulombs / meters ^ 2} si-units)
(defmetricunit farads/meters {farads / meters} si-units)
(defmetricunit henry/meters {henry / meters} si-units)
(defmetricunit joules/moles {joules / moles} si-units)
(defmetricunit joules/moles/kelvin {joules / {moles kelvin}} si-units)
(defmetricunit coulombs/kilograms {coulombs / kilograms} si-units)
(defmetricunit katals/liter {katals / liters} si-units)
(defmetricunit daltons {grams / moles} si-units)

(defmetricunit candelas/meters^2 {candelas / meters ^ 2} si-units)
(defmetricunit bimolar {molar ^ 2} b-units)
(defmetricunit trimolar {molar ^ 3} b-units)

(defmetricunit molar/seconds {molar / seconds} b-units)
(defmetricunit bimolar/seconds {bimolar / seconds} b-units)
(defmetricunit trimolar/seconds {trimolar / seconds} b-units)
(defmetricunit molar*seconds {molar seconds} b-units) ; inverse unit of unimolar-destruction rate
(defmetricunit bimolar*seconds {bimolar seconds} b-units) ; inverse unit of bimolar-destruction rate
(defmetricunit trimolar*seconds {trimolar seconds} b-units) ; inverse unit of trimolar-destruction rate

(defmetricunit watts/meters^2 {watts / meters ^ 2} b-units)

(defmetricunit hartrees  {2.625501e6 joules / moles} si-units)

;(defmetricunit lux {lumen / meters ^ 2} b-units)
;(defmetricunit lux/seq
(define celcius [[unit temperature 
                      (lambda (m inv)
                        (if inv {m + 273.15} ; input is kelvins, convert to celcius
                          {m - 273.15}))]  ; input is celcius -> convert to kelvin
                 :unit-system si-units])

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun to-unit (q us)  
  [[unit q.dimension q.base-magnitude]
   :unit-system us])

(export 'to-unit))

(define minutes (to-unit {60 * seconds} si-units))
(define hours (to-unit {60 * minutes} si-units))
(define days (to-unit {24 * hours} si-units))
(define weeks (to-unit {7 * days} si-units))

;; imperial
(define inches (to-unit {254313/100000 * centimeters} imperial-units))
(define feet (to-unit {12 * inches} imperial-units))
(define farenheit [[unit temperature
                         (lambda (m inv)
                           (if inv {{m - 32} / 9/5 + 5463/20}  ; input is kelvin, convert to farenheit
                             {9/5 * {m - 5463/20} + 32}))] ; input is farenheit, convert to kelvin
                   :unit-system imperial-units])
(define pounds (to-unit {453.59237 grams} imperial-units))
(define slug (to-unit {14.59390 kilograms} imperial-units))
(define btu (to-unit {1055 joules} imperial-units))

(define 15C-calorie (to-unit {8371/2000 joules} imperial-units))
(define ist-calorie (to-unit {10467/2500 joules} imperial-units))
(define thermochemical-calorie (to-unit {523/125 joules} imperial-units))
(define food-calorie (to-unit {4186 joules} imperial-units))
;;;
;;; Some constants...
;;;
(define avagadro [[reference-var] :value {6.02214199e23 / moles}])
(define gas-const-r [[reference-var] :value {8.314 joules / {moles kelvin}}])
(define faraday-const [[reference-var] :value {96500 coulombs / moles}])
(define planck-const [[reference-var] :value {6.6260755e-34 joules seconds}])
(define boltzmann-const [[reference-var] :value {1.380658e-23 joules / kelvin}])
(define elementary-charge [[reference-var] :value {1.60217733e-19 coulombs}])
(define speed-of-light [[reference-var] :value {2.99792458e8 meters / seconds}])
(define vacuum-permeability [[reference-var] 
                             :value {4 pi 10e-7  teslas ^ 2 meters ^ 3 / joules}])
(define vacuum-permitivity [[reference-var] :value 
                            {8.854187817e-12 coulombs ^ 2 / {joules  meters}}])
(define fine-structure-const [[reference-var] :value {1 / 137.0359895}])
(define electron-mass [[reference-var] :value {9.1093897e-31 kilograms}]) ;; rest mass
(define proton-mass [[reference-var] :value {1.6726231e-27 kilograms}]) ;; rest mass
(define neutron-mass [[reference-var] :value {1.6749286e-27 kilograms}]) ;; rest mass
(define bohr-magneton [[reference-var] :value {9.2740154e-24 joules / teslas}])
(define nuclear-magneton [[reference-var] :value {5.0507866e-27 joules / teslas}])
(define free-electron-g-factor [[reference-var] :value {2.002319304386}])
(define free-electron-gyromagnetic-ratio [[reference-var] :value {1.7608592e11 / {seconds teslas}}])
(define electron-magnetic-moment [[reference-var] :value {-9.2847701e-24 joules / teslas}])
(define proton-gyromagnetic-ratio [[reference-var] :value {2.67515255e8 /{seconds teslas}}])
(define electron-charge/mass [[reference-var] :value {1.75880e11 coulombs / kilograms}])
(define atomic-mass-unit [[reference-var] :value {1.66053886e-27 kilograms}])
(define bohr-radius [[reference-var] :value {5.29177e-11 meters}])
(define electron-radius [[reference-var] :value {2.81792e-15 meters}])
(define molar-volume [[reference-var] :value {22.41383 meters ^ 3 / kilomoles}])
(define proton-g-factor [[reference-var] :value 5.585])
(define gravity-const [[reference-var] :value {6.673e-11 meters ^ 3 / {kilograms seconds ^ 2}}])
(define gravity-acceleration [[reference-var] :value {9.80665 meters / seconds ^ 2}])
(define electron-compton-wavelength [[reference-var] :value {2.42631e-12 meters}])


