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


;;; File: reaction.lisp
;;; Description:

;;; $Id: reaction.lisp,v 1.5 2008/09/06 00:23:08 amallavarapu Exp $

(in-package #I@FOLDER)

(include @/reaction-type)

(defcon reaction ()
  ((type      reaction-type)
   (location  location))
  =>
  {.type.(in .location) :# object}
  {.reactants := (satisfy-requirements .type :lhs-requirements .location)}
  {.products := (satisfy-requirements .type :rhs-requirements .location)})

(defprop reaction.reactants (:= ()))

(defprop reaction.products (:= ()))

(defun satisfy-requirements (rxn-type side loc)
  "Ensures that species are created in loc (or sublocations of loc), as specified by the reaction-type-requirements of rxn-type"
  (handler-case (loop for req in rxn-type.,side
                      collect  (cons req
                                     [species req.species-type 
                                              loc.(sublocation req.sublocation)]))
    (error (e) (error "While computing species of ~S: ~A." rxn-type e))))
  

(defield reaction-type.in (loc)
  (lookup [reaction object loc]))
