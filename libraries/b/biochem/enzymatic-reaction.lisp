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


;;; File: enzymatic-reaction
;;; Description: describes the enzymatic-reaction concept
;;;              this is a meta-reaction which implicates 
;;;              reaction-types and enzyme substrate complexes

;;; $Id: enzymatic-reaction.lisp,v 1.8 2008/09/06 00:23:08 amallavarapu Exp $
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
