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


;;; $Id: reaction-inference.lisp,v 1.19 2008/09/06 00:23:08 amallavarapu Exp $
;;; $Name:  $

;;; Description: detects when patterns described in complex-reaction-type objects
;;;              are satisfied, and generates reaction-type objects 

(in-package #I@library/biochem)

(include (@folder/species-type @folder/reaction-type) :use :modify b/biochem)

(defrule complex-reaction-exists 
  (?cr complex-reaction-type)
  =>
  (multiple-value-bind (rule-pattern actions)
      (compute-graph-inference-rule-parts ?cr)
    (add-rule rule-pattern actions (intern (with-print-context t
                                             (format nil "~A" ?cr))))))

(defprop reaction-type.pattern-map (:= ()))

(defield complex-reaction-type.lhs-patterns ()
  (mapcar (lambda (o)
            [complex-pattern o])
          (nth-value 5 (compute-complex-reaction-type-changes object))))

(defun compute-graph-inference-rule-parts (cr)   
  "Where LHS and RHS are sum-expressions or lists of COMPLEX-PATTERNS:
   RETURNS: LHS-PATTERNS (complex-graphs representing the patterns)
            RULE-LHS
            RULE-RHS"
  (multiple-value-bind (bonds lost-bonds relabels keepers losers lhs-patterns rhs-patterns rhs-new-graph
                              crt-lhs-entities rhs-monomer-locs)
      (compute-complex-reaction-type-changes cr)
    (declare (ignorable rhs-patterns))
    (loop for p in lhs-patterns
          for gnum = 1 then (1+ gnum)
          for cstype = (intern (format nil "?CST~A" gnum)) ;; complex-species-type variable
          for ivar = (intern (format nil "?I~A" gnum)) ;; isomorphism
          collect `[complex-pattern-match ,[complex-pattern p] ,cstype ,ivar] into rule-pattern
          collect cstype into lhs-cstypes
          collect ivar into ivars
          finally (return (values ; lhs-patterns
                                  `(:and ,@rule-pattern)
                                  `(create-reaction-type-from-complex-reaction-type
                                    ,cr                  ; complex reaction
                                    ',crt-lhs-entities   ; list of localization or reference-pattern objects
                                    (list ,@lhs-cstypes) ; LHS complex-species-types in new reaction-type 
                                    ,rhs-new-graph       ; the new rhs-graphs
                                    (vector ,@ivars)     ; isomorphisms
                                    ',bonds              ; bonds to create
                                    ',lost-bonds         ; bonds to delete
                                    ',relabels           ; relabellings
                                    ',keepers            ; rhs verticies that must be kept
                                    ',losers             ; verticies to delete
                                    ',rhs-monomer-locs)  ; 
                                  )))))

(defcon complex-reaction-inference (:notrace)
  ((type complex-reaction-type)
   (instance reaction-type) 
   requirements))
  

(defun make-localized-complex-species-type (graph)
  "Given a graph containing localizations, returns a complex-species-type or a localization object"
  (loop for i from 0 below (gtools:graph-vertex-count graph)
        for lab = (gtools:graph-vertex-label graph i)
        when (localization-p lab)
        do (setf (gtools:graph-vertex-label graph i)
                 lab.entity)
        and collect lab.location into sublocations
        finally (return 
                 (cond
                  ((position nil sublocations) [complex-species-type (gtools:canonical-graph graph)])
                  ((> (length (remove-duplicates sublocations)) 1)
                   (b-error "Cannot create complex-species-type - multiple sublocations specified: ~S"
                            (remove-duplicates sublocations)))
                  (t 
                   (let* ((subloc (first sublocations))
                          (cst    [complex-species-type (gtools:canonical-graph graph)]))
                     {cst @ subloc}))))))
                     

(defun create-reaction-type-from-complex-reaction-type 
       (cr crt-lhs-entities lhs-species-types rhs-new-graph isomorphisms new-bonds lost-bonds relabels keepers losers rhs-monomer-localizations)
  (let ((lclass cr.location-class))
    (flet ((copy-localization-to-complex-species-type (lcp cst)
             (if (localization-p lcp) {cst _@ lcp.location}
               cst))
           (localization-valid-p (x)
             (etypecase x
               (species-type (eq x.location-class lclass))
               (localization x.(is-valid-for lclass)))
             ;;(b-error "~S is an invalid localization for ~S" loc.location loc.entity))))
             ))
      (with-b-error-context ("Matching ~S, with LHS = ~S"
                             cr lhs-species-types)
        (let* ((lhs-graphs (mapcar ?.id lhs-species-types))
               (localized-lhs-csts (mapcar #'copy-localization-to-complex-species-type
                                           crt-lhs-entities lhs-species-types))
               (localized-rhs-csts  (mapcar #'make-localized-complex-species-type
                                            (compute-rhs-graphs (apply #'vector
                                                                       rhs-new-graph
                                                                       lhs-graphs)
                                                                isomorphisms
                                                                new-bonds
                                                                lost-bonds
                                                                relabels
                                                                keepers
                                                                losers
                                                                rhs-monomer-localizations)))
               (rtype      (when (notany #'null localized-rhs-csts)
                             [reaction-type 
                              localized-lhs-csts
                              localized-rhs-csts
                              cr.location-class])))
          (when rtype
            [complex-reaction-inference cr rtype (mapcar #'cons crt-lhs-entities localized-lhs-csts)]))))))
