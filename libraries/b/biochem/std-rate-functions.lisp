;;;; This file is part of little b.

;;;; Copyright (c) 2005-8 Aneil Mallavarapu

;;;; Little b is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Little b is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with little b.  If not, see <http://www.gnu.org/licenses/>.

;;; File: std-rate-functions
;;; Description:


;;; $Id: std-rate-functions.lisp,v 1.8 2008/09/02 14:58:10 amallavarapu Exp $

(in-package #I@FILE)

(include-declaration :use-packages mallavar-utility
                  :expose-symbols (mass-action custom-rate mass-action-constant-dimension))

(include (@>/math 
          @>/biochem 
          @>/util
          @>/biochem/ode
          @>/biochem/dimensionalization) :use)

(define-custom-rate mass-action (constant) 
    (:rate-dimension rate-dimension
     :entities entities
     :stoichiometries stoichiometries
     :dimensions dimensions)
  (let ((mass-action-term 1) ;; initialize the mass-action term and 
        (constant-dimension rate-dimension)) ;; expected dimension of the constant

    ;; calculate the mass-action term and the constant's expected dimension
    (loop for stoich in stoichiometries
          for entity in entities
          for dimension in dimensions
          do (setf mass-action-term {mass-action-term * entity ^ stoich}
                   constant-dimension {constant-dimension / dimension ^ stoich}))

    ;; check the constant's dimension
    (unless (eq (dimension-of constant) constant-dimension)
      (cond

       ;; if it's a number, we'll warn & fix it automatically:
       ((numberp constant) 
        (setf constant {constant * constant-dimension.unit})
        (b-warn "Numeric constant give for ~S.mass-action. Using dimensional constant: ~S."
                dictionary constant))

       ;; incorrect units provided: throw an error 
       (t (b-error "Invalid units used for ~S.mass-action.  Expecting ~S."
                   dictionary constant-dimension.unit))))
    
    (store-parameter :mass-action constant)

    {:mass-action * mass-action-term}))

(define-custom-rate custom-rate (expr &rest constants) ()
  "USAGE: X.(set-rate-function 'custom-rate {mathematical-expression} :const1-name const1 :const2-name const2) where X is an object which supports has a SET-RATE-FUNCTION field"
  ;; stuff the constants in the dictionary
  (do () ((null constants))
    (let ((key (pop constants))
          (val (pop constants)))
      (store-parameter key val)))
  expr)

(define-custom-rate hill (species &key (kd 1) (hill 1)) ()
  ;; NEED TO WRITE DIMENSION CHECKING CODE
  (store-parameter :kd kd)
  (store-parameter :hill hill)
  {species ^ :hill / {species ^ :hill + :kd ^ :hill}})
  


