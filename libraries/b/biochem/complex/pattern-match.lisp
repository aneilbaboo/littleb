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

;;; $Id: pattern-match.lisp,v 1.3 2007/11/29 17:36:57 amallavarapu Exp $
;;; $Name:  $

;;; Description: Matches complex patterns in the database

(in-package #I@library/biochem)

(include @folder/species-type)

(defcon complex-pattern-match (:notrace)
  ((complex-pattern complex-pattern) ; graph representing the complex pattern
                   ; (cannot use PATTERN because this interferes w/ lisa)
   (complex-species-type complex-species-type)
   isomorphism))

(defrule detect-complex-pattern-isomorphism
  (:and (?pattern [complex-pattern ?subgraph])
        (?species-type [complex-species-type ?graph]))
  =>
  (dolist (iso (gtools:find-subgraph-isomorphisms ?subgraph ?graph))
    [complex-pattern-match ?pattern ?species-type iso]))


(defield complex-pattern.matches (x)
  (assert (or (complex-pattern-p x)
              (complex-species-type-p x)))
  (gtools:find-subgraph-isomorphisms .id x.id))