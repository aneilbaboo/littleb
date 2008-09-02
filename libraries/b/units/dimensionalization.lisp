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

(include @LIBRARY/math :expose)

(define-var *time-dimension*)
(define-var *distance-dimension*)
(define-var *mass-dimension*)
(define-var *temperature-dimension*)
(define-var *luminous-intensity-dimension*)
(define-var *current-dimension*)
(define-var *molecular-amount-dimension*)

(defconstant +dimensionalization-vars+  '(*time-dimension*
                                          *distance-dimension*
                                          *mass-dimension*
                                          *temperature-dimension*
                                          *luminous-intensity-dimension*
                                          *current-dimension*
                                          *molecular-amount-dimension*))

(define-assumption-name :dimensionalization 
  :initializer (lambda () (mapc #'makunbound +dimensionalization-vars+))
  :verifier (lambda () (every (lambda (s) (and (boundp s) (dimension-p (symbol-value s))))
                              +dimensionalization-vars+))
  :verify-error (format nil "Each of ~S must be a dimension." +dimensionalization-vars+)
  :verify-error (format nil "Ensure that each of ~S is set." +dimensionalization-vars+)
  :general-error "Ensure that exactly one of b/UNITS/NULL, b/UNITS/STANDARD or some other dimensionalization regime is included.")
