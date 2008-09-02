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
(in-package #I@FILE)

(include @>/units/dimensionalization :expose)

(include-declaration :expose-symbols (check-biochem-dimensionalization))

(include-documentation :description "Provides variables which allow control of dimensionalization of b/biochem concepts, ~
                                     and provides a default dimensionalization in which compartments have volumes and ~
                                     membranes have areas"
                       :authors ("Aneil Mallavarapu")
                       :organization ("Harvard Medical School"))

(define-var *compartment-dimensionality*)


(define-assumption-name :biochem-dimensionalization 
  :requires :dimensionalization
  :verifier (lambda () (boundp '*compartment-dimensionality*))
  :verify-error (format nil "Biochem compartment dimensionality has not been selected (*compartment-dimensionality* has not been set)")
  :initializer (lambda () (makunbound '*compartment-dimensionality*))
  :general-error (format nil "Ensure that exactly one biochem dimensionalization regime is loaded.  ~
                              e.g., b/BIOCHEM/2-DIMENSIONAL or ../3-DIMENSIONAL or ../NON-DIMENSIONAL"))

