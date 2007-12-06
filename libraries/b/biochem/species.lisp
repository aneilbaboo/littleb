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

;;; File: species
;;; Description: Defines the species concept.

(in-package #I@FOLDER)

(include (@folder/species-type @folder/location))
;;;
;;; species
;;;
(defcon species ()
  "Represents a species-type in a particular location"
  ((type           species-type :documentation "The species-type that this species is an instance of")
   (location       :documentation "A location object"))
  (when (not (typep .location .type.location-class))
    (b-error "Cannot create species of type ~S in ~S.  Location-class mismatch: expecting ~S, but received (class ~S)."
           .type .location .type.location-class (class-of .location)))
  =>
  {.type.(in .location) :# object}) ; name this object according to the type & where it is located.

(defield species-type.in (loc)
  "Returns the species of this type which exists in location LOC, or NIL"
  (or (lookup [species object loc])
      (b-error "Unable to find ~S.(in ~S)" object loc)))

(defield location.all-species ()
  (remove object (query species) :key ?.location :test-not #'eq))

(predefine (:class (species reaction))
(defield location.contains (&rest stypes)
  "Causes one or more SPECIES of type X to be generated in the location"
  (labels ((create-species (x) 
             (etypecase x
               (list         (mapcar #'create-species x))
               (species-type (list [species x object]))
               (reaction-type (list [reaction x object])))))
    (mapcan #'create-species stypes))))


(defield location.species (stype)
  (find stype .all-species :key ?.type))
