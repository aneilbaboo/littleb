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

;;; File: reaction-inference.lisp
;;; Description:  when included, this rule infers which species and reactions
;;;               are implied by an initial set of species and reactions.

;;; $Id: reaction-inference.lisp,v 1.5 2007/12/12 15:33:39 amallavarapu Exp $

(in-package #I@FILE)

(include @>/biochem :use)

(defprop reaction.substitution-table
    (:= (make-hash-table)
     :documentation "A hashtable mapping higher-level objects (e.g., species-types) to species"))

(defrule reaction-inference-generator
  (?rtype reaction-type)
  =>
  (multiple-value-bind (patterns substitution-list)
      (generate-reaction-inference-rule ?rtype '?loc)
    (add-rule patterns
              `(infer-reaction ,?rtype ?loc ,substitution-list)
              (intern (format nil "~A" ?rtype)))))

(defun infer-reaction (rtype loc substs)
  "creates a reaction and adds substitions to the substition table"
  (let* ((r [reaction rtype loc])
         (table (|REACTION.SUBSTITUTION-TABLE| r)))
    (dolist (s substs)
      (setf (gethash (car s) table) (cdr s)))
    r))

(defun generate-reaction-inference-rule (rtype mainlocvar)
  "Returns a pattern suitable for defrule, and a form which computes the list of substitutions (of entities to species"
  (let ((location-patterns `((nil ,mainlocvar (,mainlocvar [,(class-name rtype.location-class)]))))
        (subloc-counter    0))
    (flet ((get-location-var (subloc)
             (let ((existing (assoc subloc location-patterns)))
               (if existing (second existing)
                 (let* ((sublocvar (intern (format nil "?SUBLOC~A" (incf subloc-counter)))))
                   (push `(,subloc ,sublocvar [has-sublocation ,mainlocvar ,subloc ,sublocvar])
                         location-patterns)
                   sublocvar)))))   
      
      (loop for req in rtype.lhs-requirements
            for i = 1 then (1+ i)
            for species-var = (intern (format nil "?SPECIES~A" i))
            for stype = req.species-type
            for location-var = (get-location-var req.sublocation)
            collect `(,species-var [species ,stype ,location-var]) into species-patterns
            collect `(cons ,req.(localization t) ,species-var) into substitutions
            finally (return 
                     (values (list* :and
                                    (nconc (nreverse (mapcar #'third location-patterns))
                                           species-patterns))
                             
                             `(list ,@substitutions)))))))
            
        
