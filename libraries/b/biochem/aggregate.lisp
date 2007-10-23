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

;;; File: aggregate.lisp
;;; Description: an aggregate is a reaction-type whose identity is given by a
;;;              a polynomial which describes the contents of the aggregate.  
;;;              An aggregate is structureless in the sense that only the
;;;              stoichiometries of the components are important to its identity.
;;;              Order of addition and the exact connectivity of the components
;;;              is not represented in an aggregate.


;;; $Id: aggregate.lisp,v 1.2 2007/10/23 17:25:55 amallavarapu Exp $
;;;

(in-package #I@FILE)

(include-declaration :use-packages (mallavar-utility))

(include (@</simple-ode-modelling
          @</math) :use)

;;;;
;;;; simple-aggregate - multimer formed by a single reversible reaction
;;;; 
(defcon simple-aggregate ()
  ((components sum-expression) 
   &property (binding :#= [reversible-reaction .components object .location-class])))


(defield simple-aggregate.kf ()
  .binding.kf)

(defield (setf simple-aggregate.kf) (value)
  {.binding.kf := value})

(defield simple-aggregate.kr ()
  .binding.kr)

(defield (setf simple-aggregate.kr) (value)
  {.binding.kr := value})


;;;;
;;;; dimerizing-aggergate - in which the aggregate is assembled from all possible 
;;;;                 pairwise interactions of components of the multimer.    There is
;;;;                 no additional "structure" implied by the multimer other than the
;;;;                 
;;;;                 
;;;;
(defcon dimerizing-aggregate ()
  ((components sum-expression) 
   &property (forward-reactions) (reverse-reactions))

  {.location-class := (determine-dimerizing-aggregate-location-class .components)}
  =>
  (let ((lhses (compute-dimerizing-aggregate-reaction-lhses .components .location-class)))
    {.forward-reactions 
     := (mapcar (lambda (lhs)
                  [reaction-type lhs {object} .location-class])
                lhses)}
    {.reverse-reactions 
     := (mapcar (lambda (lhs)
                  [reaction-type {object} lhs .location-class])
                lhses)}))

(defun determine-dimerizing-aggregate-location-class (components)
  (find-if (lambda (v n) (if (species-type-p v) v.location-class))
           components.variable))

(defield dimerizing-aggregate.fwd (components)
  (find components .forward-reactions :key ?.lhs))

(defield dimerizing-aggregate.rev (components)
  (find components .reverse-reactions :key ?.rhs))


;;;; (defmethod dimerizing-aggregate.kf (components)
;;;;   (.rxn components).kf)

;;;; (defmethod dimerizing-aggregate.kr (components)
;;;;   (.rxn components).kr)
;;;;   
(defield (setf dimerizing-aggregate.fwd-ks) (value &optional signal &rest args)
  (set-dimerizing-aggregate-reaction-const object :fwd signal args value))

(defield (setf dimerizing-aggregate.rev-ks) (value &optional signal &rest args)
  (set-dimerizing-aggregate-reaction-const object :rev signal args value))

(defun compute-dimerizing-aggregate-reaction-lhses (components loc-class)  
  (let ((monomer-pairs (poly-compute-pairwise-positive-integer-combinations components)))
    (mapcar (lambda (mpair) {(reaction-requirement-from-dimerizing-aggregate-components 
                              (first mpair) loc-class) +
                             (reaction-requirement-from-dimerizing-aggregate-components
                              (second mpair) loc-class)})
            monomer-pairs)))

(defun reaction-requirement-from-dimerizing-aggregate-components (s loc-class)
  "Given a poly returns a species-type or dimerizing-aggregate object, depending on
how many species-types are in the poly"
  (case (count-vars-in-sum-expression s)
    (1            (car (first s.variable)))
    (otherwise    (let ((vars p.vars))
                    (if (every #'location-requirement-p vars)
                        [dimerizing-aggregate p.(map ?.type)].(required (first vars).localization)
                      [dimerizing-aggregate p])))))
                             
(defun poly-compute-pairwise-positive-integer-combinations (components)
  "Given a polynomial composed of variables with positive integer coefficients, generates
a list of all pairs of polynomials which could be added together to produce poly.  
e.g., (poly-co... {a + b}) => (({a} {b})) (poly-co... {a + 2 b}) => (({a + b} {b}) ({a} {2 b}))
Current implementation is ugly and slow, but works"
  (labels ((inner-loop (p extra)
             (loop with vars = components.variable
                   for  v in vars
                   for  c1 = {v + extra}
                   for  c2 = {p - v}
                   collect (order (list c1 c2))
                   when (> (count-vars-in-sum-expression c2) 1)
                   nconc (inner-loop c2 c1))))
    (remove-duplicates (inner-loop poly 0) :test #'equal)))

(defun count-vars-in-sum-expression (s)
  {s.(map (constantly t)) / t})

;;;;
;;;; starting point for a more efficient approach, but this doesn't work:
;;;; (defun poly-compute-pairwise-positive-integer-combinations (p &optional (extra 0))
;;;;   (let* ((v         p.main-var)
;;;;          (c1        {v + extra})
;;;;          (c2        {p - v}))
;;;;     (cons (list c1 c2)            
;;;;           (when (> (poly-sum-var-coefficients c2) 1)
;;;;             (poly-compute-pairwise-positive-integer-combinations c2 c1)))))

;;;; (defun poly-sum-var-coefficients (p)
;;;;   {p.(map (constantly t)) / t})




(defun set-dimerizing-aggregate-reaction-const (ag rxn-dir signal args value)
  (flet ((reactions-from-direction ()
           (ecase rxn-dir
             (:fwd ag.forward-reactions)
             (:rev ag.reverse-reactions)))
         (reaction-vars-from-direction (rxn)
           (ecase rxn-dir
             (:fwd (the reaction-type rxn).lhs.vars)
             (:rev (the reaction-type rxn).rhs.vars))))
    (case signal
      ((t :all)     (set-dimerizing-aggregate-reaction-const ag rxn-dir nil args value)
                    (mapc (lambda (rxn) 
                            (aif (find-if (lambda (o) (or (dimerizing-aggregate-p o)
                                                          (and (location-requirement-p o)
                                                               (dimerizing-aggregate-p o.type))))
                                              (reaction-vars-from-direction rxn))
                                (set-dimerizing-aggregate-reaction-const 
                                 (if (location-requirement-p it) it.type it)
                                 rxn-dir signal args value)))
                          (reactions-from-direction)))
      ((nil)        (mapc (lambda (rxn)
                            (setf rxn.k value))
                          (reactions-from-direction)))
      (:lhs-contains (mapc (lambda (rxn)
                             (let ((all-rtypes    (reaction-vars-from-direction rxn))
                                   (rtype        (first args)))
                               (aif (find rtype all-rtypes)
                                   (set-dimerizing-aggregate-reaction-const ag rxn-dir rxn.lhs nil value))
                               (aif (find-if #'dimerizing-aggregate-p all-rtypes)
                                   (set-dimerizing-aggregate-reaction-const
                                    ag rxn-dir :lhs-contains args value))))
                           (reactions-from-direction)))
      (otherwise            (unless (species-type-p signal)
                              (b-error "When setting ~S.(~A ~S), ~S is not a species-type or one of ~
                              (T :ALL NIL :LHS-CONTAINS)" ag rxn-dir signal signal)
                              (setf ag.(rxn signal).,rxn-dir value))))
    value))
  