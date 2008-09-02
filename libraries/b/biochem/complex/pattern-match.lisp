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

;;; $Id: pattern-match.lisp,v 1.5 2008/09/02 14:58:10 amallavarapu Exp $
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
  (dolist (iso (gtools:find-subgraph-isomorphisms 
                ?subgraph ?graph 
                :vertex-test (make-complex-graph-vertex-test-fn ?subgraph ?graph)
                :row-fn (make-complex-graph-row-fn ?subgraph ?graph)))
    [complex-pattern-match ?pattern ?species-type iso]))


(defield complex-pattern.matches (x)
  (assert (or (complex-pattern-p x)
              (complex-species-type-p x)))
  (gtools:find-subgraph-isomorphisms .id x.id))