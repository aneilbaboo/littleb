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


;;; File: graph.lisp
;;; Description: Graph objects where identity is maintained according
;;;              to isomorphism.


;;; $Id: graph.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $

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
                   
    
