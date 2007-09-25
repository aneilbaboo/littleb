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

;;; File: core-biochem
;;; Description: defines species-type, species, location, 

;;; $Id: biochem.lisp,v 1.1 2007/09/25 17:54:02 amallavarapu Exp $

(in-package #I@file)

(include-declaration :expose-symbols (location-class-dimension)
                     :use-packages (mallavar-utility))
 
(include-documentation
  :description "Includes the core concepts for the biochemistry system.~
                Note: This file does not contain bindings for dimensionalization - ~
                use </2-dimensional or </3-dimensional or </non-dimensional instead.  ~
                $Date: 2007/09/25 17:54:02 $"
  :authors "Aneil Mallavarapu"
  :organization "Harvard Medical School")

(include (@>/math))

(include (@/species-type
          @/species 
          @/location 
          @/species-type 
          @/reaction 
          @/reaction-type
          @/dimensionalization) :expose)

