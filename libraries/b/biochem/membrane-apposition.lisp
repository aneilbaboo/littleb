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

(include (@>/biochem) :expose)
(include @library/biochem/dimensionalization)

(defcon membrane-apposition (:notrace location)
  "A relationship between two membranes which states that they are on opposite sides of an interstitial compartment and that they interact with each other through reactions."
  ((m1 membrane)
   (m2 membrane)
   &property 
   (c1 (allow compartment) := .m1.inner)
   (c2 (allow compartment) := .m2.inner)
   (ic (allow compartment) :#= [compartment])
   (inverse))
  =>
 {.size.value := .m1.size.value}
  (unless (or (nil-or-? .m1.apposition) 
              (nil-or-? .m2.apposition))
    (check-apposition-membranes .m1 .m2))
  {.m1.outer := .ic}
  {.m2.outer := .ic}
  {.m1.apposition := object}
  (let ((c1 .c1)
        (c2 .c2)
        (ic .ic))
    {.inverse := [[membrane-apposition .m2 .m1 ] :c1 c2 :c2 c1 :ic ic]}))

(defun nil-or-? (o)
  (or (null o) (missingp o)))

(defun membrane-apposition (membrane-1 membrane-2 &optional (side-length 1))
  (declare (ignorable cell1 cell2))
  (let ((ma [membrane-apposition membrane-1 membrane-2]))
    (with-fields ma
      {.size.value := side-length}
      {membrane-1.size.value := side-length}
      {membrane-1.inverse.size.value := side-length}
      {membrane-2.size.value := side-length}
      {membrane-2.inverse.size.value := side-length})
    ma))

(defun check-apposition-membranes (m1 m2)
  (let* ((m1a m1.apposition)
         (m2a m2.apposition))
    (when (eq m1 m2) (b-error "Same membrane (~S) passed as M1 M2 arguments for MEMBRANE-APPOSITION."
                            m1))
    (unless (or (null m1a) (null m2a)
                (and (eq m1 m1a.m1) (eq m1 m2a.m1) (eq m2 m1a.m2) (eq m2 m2a.m2)))
      (b-error "These membranes may not be apposed (~S ~S).  ~
              (They might already be apposed to other membranes." m1 m2))))
      


(defprop membrane.apposition (:= nil))

(defmethod location-class-dimensionality ((lc (eql membrane-apposition))) 
  (1- *compartment-dimensionality*))
