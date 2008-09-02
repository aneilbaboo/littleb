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

;;; File: core-biochem
;;; Description: defines species-type, species, location, 

;;; $Id: biochem.lisp,v 1.4 2008/09/02 14:58:09 amallavarapu Exp $

(in-package #I@file)

(include-declaration :expose-symbols (location-class-dimension)
                     :use-packages (mallavar-utility))
 
(include-documentation
  :description "Includes the core concepts for the biochemistry system.~
                Note: This file does not contain bindings for dimensionalization - ~
                use </2-dimensional or </3-dimensional or </non-dimensional instead.  ~
                $Date: 2008/09/02 14:58:09 $"
  :authors "Aneil Mallavarapu"
  :organization "Harvard Medical School")

(include (@>/math))

(include (@/species-type
          @/species 
          @/location 
          @/species-type 
          @/reaction 
          @/reaction-type
          @/dimensionalization
          @/enzymatic-reaction
          @/complex
          ) :expose)

