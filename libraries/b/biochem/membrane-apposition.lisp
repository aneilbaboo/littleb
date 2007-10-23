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

(in-package #I@FILE)

(include (@>/biochem) :expose)
(include @library/biochem/dimensionalization)

(defcon membrane-apposition (location)
  "A relationship between two membranes which states that they are on opposite sides of an interstitial compartment and that they interact with each other through reactions."
  ((m1 membrane)
   (m2 membrane)
   &property 
   (c1 (allow compartment) := .m1.c2)
   (c2 (allow compartment) := .m2.c2)
   (ic (allow compartment) :#= [compartment])
   (inverse))
  =>
 {.size.value := .m1.size.value}
  (unless (or (nil-or-? .m1.apposition) 
              (nil-or-? .m2.apposition))
    (check-apposition-membranes .m1 .m2))
  {.m1.c1 := .ic}
  {.m2.c1 := .ic}
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

(defmethod location-class-dimension ((lc (eql membrane-apposition))) *membrane-size-dimension*)
