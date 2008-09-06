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


;;; File: species-type
;;; Description: Defines the species-type concept. 

;;; $Id: species-type.lisp,v 1.6 2008/09/06 00:23:08 amallavarapu Exp $

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
