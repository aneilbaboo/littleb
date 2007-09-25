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

;;; File: species-type
;;; Description: Defines the species-type concept. 

;;; $Id: species-type.lisp,v 1.1 2007/09/25 17:54:03 amallavarapu Exp $

(in-package #I@FOLDER)

(include @/location) 

(defcon species-type () 
  "Represents a chemically distinct molecular entity" 
  (&optional (ID := *name*)
   &property (location-class    location-class := compartment
                                :documentation "The class of location in which species of this type can go")
             (species := (make-hash-table) :relevance t
                      :documentation "A hashtable of species, for internal use")))

(defield species-type.required (subloc)
  "Returns an object which represents a requirement for a species in a relative location.  SUBLOC is a keyword which names a field of a location which points to another location."
  (has-name object.(required subloc) [location-requirement object subloc]))

(defield species-type.in (loc)
  "Returns the species of this type which exists in location LOC, or NIL"
  (gethash loc .species))


(define-macro def-species-types (location-class &body stype-defs)
 "Defines a bunch of species in a particular location class (membrane, compartment or cell).
  USAGE:  (DEF-SPECIES-TYPES location-class stype-def*)
          where stype-def is either a symbol representing a species, or
                a list matching: (name &optional documentation &key props)"
  `(progn ,@(loop for spec in stype-defs
                  for fixed-spec = (if (listp spec) spec (list spec))
                  collect (destructuring-bind (symbol &rest args) fixed-spec
                            `(define ,symbol [[species-type] 
                                              ,@(if location-class `(:location-class ,location-class))
                                              ,@(if (stringp (first args)) '(:documentation))
                                              ,@args])))))
