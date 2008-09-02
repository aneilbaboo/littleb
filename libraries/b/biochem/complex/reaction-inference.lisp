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

;;; $Id: reaction-inference.lisp,v 1.18 2008/09/02 14:58:10 amallavarapu Exp $
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
