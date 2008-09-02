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

;;; File: reaction.lisp
;;; Description:

;;; $Id: reaction.lisp,v 1.4 2008/09/02 14:58:10 amallavarapu Exp $

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
