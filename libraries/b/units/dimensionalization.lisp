;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2003-2008 Aneil Mallavarapu

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
