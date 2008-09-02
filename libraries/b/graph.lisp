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

;;; File: graph.lisp
;;; Description: Graph objects where identity is maintained according
;;;              to isomorphism.


;;; $Id: graph.lisp,v 1.2 2008/09/02 14:58:09 amallavarapu Exp $

(in-package #I@FILE)

(include-declaration :use-packages (mallavar-utility))

(include @/ullman)

(defconstant +graphs+ (make-hash-table :test 'equalp))

(defcon graph () 
  (&key points matrix)
  (let* ((points        (or .points (array-dimension .matrix 0)))
         (matrix        (ensure-square-bit-matrix .matrix))
         (graph-key     (etypecase points
                          (cons    (order points))
                          (number  points)))
         (gfields       (gethash graph-key +graphs+)) ;; (fields of) graphs which may be isomorphic         
         (graph-fields  (cond
                         (gfields (or (find-isomorphic-graph points matrix gfields)
                                      (add-graph graph-key points matrix)))
                         (t       (add-graph graph-key points matrix)))))
    graph-fields))

(defun ensure-square-bit-matrix (m)
  (let ((type (array-element-type m))
        (dim1 (Array-Dimension m 1))
        (dim2 (array-dimension m 2)))
    (assert (eq dim1 dim2) () "Error: [graph ... ~S] a square matrix is required" m)
    (if (equalp type '(unsigned bit 1)) m
      (make-array (list dim1 dim2) :element-type 'bit :initial-contents m))))
    
(defun add-graph (key points matrix)
  (push (list :points points :matrix matrix) (gethash key +graphs+)))

(defun find-isomorphic-graph (points matrix gfields)
  (etypecase points
    (number  (find-unlabelled-graph matrix gfields))
    (cons    (find-labelled-graph points matrix gfields))))

(defun find-labelled-graph (points matrix gfields)
  (loop for gf in gfields
        for gf-points = (getf gf :points)
        for gf-matrix = (getf gf :matrix)
        do (ifit (find-labelled-subgraph-isomorphisms
                 points matrix gf-points gf-matrix :find-all nil)
                 (return it))))
    
(defun find-unlabelled-graph (matrix gfields)
  (dolist (gf gfields)
    (ifit (find-subgraph-isomorphisms matrix (getf gf :matrix) :find-all nil)
        (return it))))
                   
    
