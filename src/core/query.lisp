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

;;; File: query
;;; Description: support for queries

;;;              given database with [x 1 2] [x 3 4]  [x 5 6],

;;;              (query var pattern) => list of bindings for var
;;;              e.g., (query ?a [x ?a]) => (1 3 5) 
;;;              (query vars pattern) => list of lists of bindings for vars
;;;              e.g., (query (?a ?b) [x ?a ?b]) => ((1 2) (3 4) (5 6))

;;;              shorthands:

;;;              query all elements of a class:
;;;              (query x) == (query ?obj [x]) => ([x 1 2] [x 3 4] [x 5 6])
;;;               
;;;              query pattern with only 1 pattern var:
;;;              (query [x ?a]) == (query ?a [x ?a])
;;;
;;;              query pattern with >1 pattern var:
;;;              (query [x ?a ?b]) == (query (?a ?b) [x ?a ?b]) ; pattern vars in order encountered
;;;              

;;; $Id: query.lisp,v 1.5 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(defmacro lookup (object)
  "Query a specific object without pattern matching"
  `(with-fast-query-object ,object))
  
(defmacro query (head &optional (body nil body-p))
  (cond
   ;; (query [myobject...])
   ((not body-p)
    (ifit (if (consp head) (extract-pattern-vars head))
        `(query ,(if (> (length it) 1) it (first it)) ,head)
      (let ((var (gentemp "?O")))
        `(query ,var (,var ,head))))) ;; abbrev form -> value-pattern
   
   ;; (query ?var ...patterns containing ?var...)
   ((or (pattern-var-p head)
        (and (consp head)
             (every #'pattern-var-p head)))
    (build-query head body))
   
   (t (b-error "Invalid query format: (QUERY ~S~@[ ~S~]).  Try (query simple-pattern) or ~
                  (query (?v1 ?v2 ...) complex-pattern)"
               head body))))


(defvar *query-results*)

(defun build-query (vars patterns)
  (let* ((binding-form       (if (consp vars) (multivar-binding-form vars) vars))
         (rule-parse         (make-rule-parse patterns))
         (rhs-code           `(push ,binding-form *query-results*))
         (lisa-defs          (rule-parse-lisa-rule-definitions rule-parse nil rhs-code)))
    `(lisa:with-inference-engine ((lisa:make-inference-engine))
       (lisa:reset)
       (let* ((*query-results* nil)
              (rules           (list ,@lisa-defs)))
         (assert-class-instances ',(rule-parse-classes rule-parse))
         (lisa:run)
         (dolist (r rules) (lisa:undefrule (lisa:rule-name r)))
         *query-results*))))


(defun assert-class-instances (classes)  
  (let ((rtype-expr `(or ,@(mapcar #'class-name classes))))
    (loop for obj being the hash-values of +objects+
      ;for type = (type-of obj)
      when (typep obj rtype-expr) ;(some (lambda (rtype) (subtypep rtype type)) rule-types)
      do (lisa:assert-instance obj)

      ;; now, check all of the properties of this object...
      when (conceptp obj)
      do  (loop for prop being the hash-values of (concept-properties obj)
           when (typep prop rtype-expr) ;; if this property matches type...
           do (lisa:assert-instance prop))))) ;; assert it
           
    
(defun multivar-binding-form (vars)
  `(list ; ,@(mapcar (lambda (v) `(cons ',v ,v)) 
    ,@vars))
