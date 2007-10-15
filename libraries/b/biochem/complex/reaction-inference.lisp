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

;;; $Id: reaction-inference.lisp,v 1.2 2007/10/15 12:48:50 amallavarapu Exp $
;;; $Name:  $

;;; Description: detects when patterns described in complex-reaction-type objects
;;;              are satisfied, and generates reaction-type objects 

(in-package :b-user)

(include (@folder/reaction-type @folder/pattern-match))

(defrule complex-reaction-exists 
  (?cr complex-reaction-type)
  =>
  (multiple-value-bind (graph-patterns rule-pattern actions)
      (compute-graph-inference-rule-parts ?cr)
    (dolist (g graph-patterns)
      [complex-pattern g])
    (add-rule rule-pattern actions)))


(defprop reaction-type.pattern-map (:relevance t))

(defun compute-graph-inference-rule-parts (cr)   
  "Where LHS and RHS are sum-expressions or lists of COMPLEX-PATTERNS:
   RETURNS: LHS-PATTERNS (complex-graphs representing the patterns)
            RULE-LHS
            RULE-RHS"
  (multiple-value-bind (bonds lost-bonds relabels deletions lhs-patterns rhs-patterns rhs-new-graph)
      (compute-complex-reaction-type-changes cr)
    (declare (ignorable rhs-patterns))
    (loop with lhs-graphs = '#:lhs-graphs
          with lhs-cplxes = '#:lhs-cplxes
          for p in lhs-patterns
          for gnum = 1 then (1+ gnum)
          for cstype = (intern (format nil "?CST~A" gnum)) ;; complex-species-type variable
          for ivar = (intern (format nil "?I~A" gnum)) ;; isomorphism
          collect `[complex-pattern-match ,[complex-pattern p] ,cstype ,ivar] into rule-pattern
          collect cstype into lhs-cstypes
          collect ivar into ivars
          finally return (values lhs-patterns
                                 `(:and ,@rule-pattern)
                                 `(let* ((,lhs-cplxes (list ,@lhs-cstypes))
                                         (,lhs-graphs (mapcar ?.id ,lhs-cplxes)))
                                    [[reaction-type ,lhs-cplxes
                                                    (make-complexes
                                                     (compute-rhs-graphs (apply #'vector
                                                                          ,rhs-new-graph
                                                                          ,lhs-graphs)
                                                                         (vector ,@ivars)
                                                                         ',bonds
                                                                         ',lost-bonds
                                                                         ',relabels
                                                                         ',deletions))]
                                     :pattern-map (mapcar #'cons ,lhs-cplxes ,lhs-graphs)])))))