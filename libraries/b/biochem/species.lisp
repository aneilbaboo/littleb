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
    (b-error "Invalid location: ~S is invalid for ~S (expecting a location of type ~S)."
           .location  .type .type.location-class))
  =>
  {.type.(in .location) :# object}) ; name this object according to the type & where it is located.

(defield species-type.in (loc)
  "Returns the species of this type which exists in location LOC, or NIL"
  (or (lookup [species object loc])
      (b-error "Species doesn't exist: attempt to access ~S.(in ~S)." object loc)))

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
