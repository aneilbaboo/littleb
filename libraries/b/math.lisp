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

(in-package #I@file)

(include-declaration
 :expose-symbols (;; math-system: types
                  NUMERIC EXPRESSION NUMERICP NUMERIC-CONCEPT-P
                  DIMENSION-OF 
                  ;; math system: fns, macros, operators
                  ZERO-NUMERIC-P NO-DIMENSION-P EXPAND
                  ADD-OPERATOR ENSURE-REFERENCE-VAR ; delete-operator
                  + - ^ * / = < > <= >=                  ; some are redundant, but here for completeness
                  S+ S- S* S/ S^ S= S< S> S<= S>=       ; functions with &rest args, also convert to math-objects
                  DEFER CHECK-VAR-NUMERIC-FIELD-WITH-RESTARTS
                  VAR-CHECK-DIMENSION
                  ;; math-system: metric stuff
                  *MAJOR-ORDERS-OF-MAGNITUDE* 
                  *ALL-ORDERS-OF-MAGNITUDE*
                  DEFMETRICUNIT 
                  DIMENSION-COMBINATION-ERROR
                  ;; math-system: non-dimensionalization
                  WITH-DIMENSIONLESS-MATH               ; computes mathematics non-dimensionally
                  *DEFAULT-NON-DIMENSIONALIZING-UNITS*  ; a list of base units 
                  DIMENSIONLESS                         ; computes a dimensionless value
                  FORCE-DIMENSIONAL-MATH             ; turns off non-dimensionalization
                  NON-DIMENSIONALIZE-P                   ; is the non-dimensionalizing system on?
                  <G=; subgraph isomorphism
                  <G=op
                  DIMENSION-OF MAGNITUDE-OF)
 :export-symbols  (*op +op /op =op -op ^op <op >op <=op >=op
                  ;; print system
                  print-math default-math-printer pprint-infix-form pprint-math-form) 
 :use-packages    (mallavar-utility))

(include (@/reader
          @/math-concept @/math-expression @/sum-expression @/product-expression @/dtype
          @/math-printer @/sum-expression-printer @/product-expression-printer 
          @/dimension @/unit  @/non-dimensionalize 
          @/dtype
          @/quantity @/operators
          @/defer-expression @/var 
          @/gauss @/expand @/dimensional-methods))

(b::package-mark-item-clearable *package* (lambda () 
                                            (b::set-default-curly-brace-reader *working-readtable*)
                                            (b::set-default-curly-brace-reader *readtable*)))         