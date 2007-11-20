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

;;; $Id: query.lisp,v 1.4 2007/11/20 18:23:21 amallavarapu Exp $
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
