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

;;; File: species-type
;;; Description: Defines the species-type concept. 

;;; $Id: species-type.lisp,v 1.5 2008/09/02 14:58:10 amallavarapu Exp $

(in-package #I@FOLDER)

(include (@/location @/localization))

(defcon species-type () 
  "Represents a chemically distinct molecular entity" 
  (&optional (ID := *name*)
   &property (location-class    location-class := compartment
                                :documentation "The class of location in which species of this type can go")))


(define-macro def-species-types (&body defs)
 "Defines a bunch of species in a particular location class (membrane, compartment or cell).
  USAGE:  (DEF-SPECIES-TYPES location-class stype-def*)
          where stype-def is either a symbol representing a species, or
                a list matching: (name &optional documentation &key props)"
  `(progn ,@(loop with location-class
                  for spec in defs
                  for fixed-spec = (if (listp spec) spec (list spec))
                  if (and (symbolp spec) (ignore-errors (subtypep spec 'location)))
                  do (setf location-class spec)
                  else
                  collect (destructuring-bind (symbol &rest args) fixed-spec
                            `(define ,symbol [[species-type] 
                                              ,@(if location-class `(:location-class ,location-class))
                                              ,@(if (stringp (first args)) '(:documentation))
                                              ,@args])))))
