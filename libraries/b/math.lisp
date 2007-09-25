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
                  print-math default-math-printer) 
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