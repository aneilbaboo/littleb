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

(include @>/units/dimensionalization :expose)

(include-declaration :expose-symbols (check-biochem-dimensionalization))

(include-documentation :description "Provides variables which allow control of dimensionalization of b/biochem concepts, ~
                                     and provides a default dimensionalization in which compartments have volumes and ~
                                     membranes have areas"
                       :authors ("Aneil Mallavarapu")
                       :organization ("Harvard Medical School"))

(define-var *compartment-size-dimension*)
(define-var *membrane-size-dimension*)

(define-constant +biochem-dimensionalization-vars+ '(*compartment-size-dimension* *membrane-size-dimension*))

(define-assumption-name :biochem-dimensionalization 
  :requires :dimensionalization
  :verifier (lambda () (every (lambda (o) (and (boundp o) (dimension-p (symbol-value o))))
                              +biochem-dimensionalization-vars+))
  :verify-error (format nil "Each of ~S must be set to a dimension." +biochem-dimensionalization-vars+)
  :initializer (lambda () (mapc #'makunbound +biochem-dimensionalization-vars+))
  :general-error (format nil "Ensure that exactly one biochem dimensionalization regime is loaded.  ~
                              e.g., b/BIOCHEM/2-DIMENSIONAL or ../3-DIMENSIONAL or ../NON-DIMENSIONAL"))

