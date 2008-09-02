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

;;; File: enzymatic-reaction
;;; Description: describes the enzymatic-reaction concept
;;;              this is a meta-reaction which implicates 
;;;              reaction-types and enzyme substrate complexes

;;; $Id: enzymatic-reaction.lisp,v 1.7 2008/09/02 14:58:10 amallavarapu Exp $
;;;
;;;
(in-package #I@folder)
 
(include (@>/math
          @>/util
          @/species-type @/reaction-type) :expose)

(defcon enzymatic-reaction ()
  ((e :documentation "Enzyme which catalyzes the reaction")
   (s :documentation "Substrate of the reaction")
   (p :documentation "Product of the reaction")
   &optional 
   (location-class (allow concept-class) := nil)
   &property
   (complex-formation := 'enzymatic-reaction-complex-formation 
            :documentation "A function which computes a species-type representing the ES complex.  Built-in options are ENZYMATIC-REACTION-COMPLEX-FORMATION and ENZYMATIC-REACTION-SUBSTRATE-COMPLEX-FORMATION." :relevance t)
   (es        dictionary :#= [dictionary] :relevance t
              :documentation "A dictionary containing enzyme-substrate complexes, accessible by integer step numbers")
   (fwd       dictionary :#= [dictionary] :relevance t
              :documentation "A dictionary of REACTION-TYPEs representing forward steps.")
   (rev       dictionary :#= [dictionary] :relevance t
              :documentation "A dictionary of REACTION-TYPEs representing reverse steps.")))

(defprop enzymatic-reaction.steps (list :documentation "A list of the form ([:REVERSIBLE | :IRREVERSIBLE]*)" :relevance t)  
  (when (and (zerop .fwd._length) (zerop .rev._length))
    (compute-enzymatic-reaction-steps object value))
  value)

(defun enzymatic-reaction-es-complex-description (erxn n)
  (with-print-context t (format nil "Enzyme substrate complex resulting from step ~A of ~S" n erxn)))

(export 'enzymatic-reaction-complex-formation)
(defun enzymatic-reaction-complex-formation (e-rxn n)
  "Computes a species-type representing the nth complex of enzymatic reaction E-RXN, which is unique for E-RXN"
  [[species-type `.e-rxn.es.,n] :location-class e-rxn.location-class 
   :documentation (enzymatic-reaction-es-complex-description e-rxn n)])

(export 'enzymatic-reaction-substrate-complex-formation)
(defun enzymatic-reaction-substrate-complex-formation (e-rxn n)
  "Computes a species-type representing the nth complex of enzymatic reaction E-RXN, which is unique for the substrates of E-RXN"
  [[species-type `(,e-rxn.lhs ,n)] :location-class e-rxn.location-class
   :documentation (enzymatic-reaction-es-complex-description e-rxn n)])

(predefine (:class simple-aggregate)
(defun aggregate-complex-formation (species loc-class)
  [[simple-aggregate species] :location-class loc-class]))

(defun compute-enzymatic-reaction-steps (e-rxn step-descriptions)
  (let* ((e            e-rxn.e)
         (s            e-rxn.s)
         (p            e-rxn.p)
         (loc-class    e-rxn.location-class))
    (flet ((compute-rhs (lhs n) ;; computes the RHS of the rxn at step n from lhs
             (declare (ignore lhs))
             (kb-transaction {e-rxn.es.,n :#= (funcall e-rxn.complex-formation e-rxn n)}))
           (compute-rxn-step (lhs rhs n descr)
             (assert (member descr '(:reversible :irreversible)) ()
                 "Invalid reaction step description ~S.  ~
                  Expecting one of (:REVERSIBLE :IRREVERSIBLE" descr)
             (kb-transaction {e-rxn.fwd.,n :#= [reaction-type {lhs} {rhs} loc-class]})
             (when (eq descr :reversible)
               (kb-transaction {e-rxn.rev.,n :#= [reaction-type {rhs} {lhs} loc-class]}))))

      (loop with num-steps = (length step-descriptions)
            with rhs = {e + s}
            for step = 1 then (1+ step)
            for step-descr in step-descriptions
            for lhs = rhs then rhs 
            while (< step num-steps)
            do  (setf rhs (compute-rhs lhs step))
                (compute-rxn-step lhs rhs step step-descr)
            finally (compute-rxn-step rhs {e + p} step
                                      (first (last step-descriptions)))))))

(defield enzymatic-reaction.set-rate-function (fn &key fwd rev)
  (flet ((ensure-list (x) (if (listp x) x (list x))))
    (loop with fwd-rtypes = .fwd
          with rev-rtypes = .rev
          initially (when (or {fwd-rtypes._length > 0} 
                              {rev-rtypes._length > 0})
                      (compute-enzymatic-reaction-steps object .steps))
          for i from 1 to .fwd._length
          for fwd-params = fwd then (rest fwd)
          for rev-params = rev then (rest rev)
          for fwd-arg = (first fwd-params)
          for rev-arg = (first rev-params)
          for fwd-rtype = fwd-rtypes.(_try-key i)
          for rev-rtype = rev-rtypes.(_try-key i)
          when fwd-rtype do
          (format t "DOING: ~S~%" `fwd-rtype.(apply :set-rate-function ,fn (ensure-list ,fwd-arg)))
          fwd-rtype.(apply :set-rate-function fn (ensure-list fwd-arg))

          when rev-rtype do

          (format t "DOING: ~S~%" `rev-rtype.(apply :set-rate-function ,fn (ensure-list ,fwd-arg)))
          rev-rtype.(apply :set-rate-function fn (ensure-list rev-arg)) )))
