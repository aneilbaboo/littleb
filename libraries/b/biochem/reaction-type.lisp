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

;;; File: reaction-type.lisp
;;; Description:  

;;; $Id: reaction-type.lisp,v 1.4 2007/10/25 03:58:00 amallavarapu Exp $

(in-package #I@library/biochem)

(include @>/math :use)
(include (@/location @/species @/localization))

(include-documentation :description "Describes REACTION-TYPE concepts.")

(defcon reaction-type ()
  ((lhs :documentation "Left hand side components") ; {S1 RT1 + S2 RT2 ...}:   LHS and RHS are sum-expressions
   (rhs :documentation "Right hand side components") ; Sn = integer stoichiometry and RTn = species-type or localization-requirement
   &optional (location-class concept-class := nil)
   &property 
   (lhs-requirements := ()) ; a list of reaction-type-requirement objects
   (rhs-requirements := ()) ; " "    "  "         "           "
   (reactants  := (make-hash-table) :relevance t) ; key = location, val = assoc list of cons pairs where
   (products  := (make-hash-table) :relevance t)) ;   (rtr . rct) === (reaction-type-requirement . species)
  (unless (math-expression-p lhs) (setf .lhs (if (listp lhs) (apply #'s+ lhs)  {lhs})))
  (unless (math-expression-p rhs) (setf .rhs (if (listp lhs) (apply #'s+ rhs) {rhs})))
  {.location-class := (determine-reaction-type-location-class location-class .lhs .rhs)}
  =>
  (with-relevance t
    {.lhs-requirements := (create-reaction-type-requirements object :lhs .lhs)}
    {.rhs-requirements := (create-reaction-type-requirements object :rhs .rhs)}))

(defmethod print-concept ((rt reaction-type) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression rt stream)))

(defmethod print-math-expression ((rt reaction-type) &optional 
                                  (stream *standard-output*) (outer-op t))
  (with-print-context rt
    (let ((lhs-printable (if (find-if #'species-type-p rt.lhs.vars) rt.lhs
                           {rt.lhs @ rt.location-class})))
      (pprint-math-form `{,lhs-printable -> ,rt.rhs} stream outer-op))))


(defield species-type.required (subloc)
  "Returns an object which represents a requirement for a species in a relative location.  SUBLOC is a keyword which names a field of a location which points to another location."
  (b-warn "~S.(REQUIRED ~S) is deprecated: use ~0@*{~S @ ~S}" object subloc)
  {object @ subloc})
;;;
;;; reaction-type-requirement - 
;;;
(defcon reaction-type-requirement ()
  (reaction-type         
    side                   ; :lhs or :rhs
    species-type           ; species-type required
    sublocation           ; nil or a symbol denoting a field of a location
    stoichiometry))       ; stoichiometry required.

(defield reaction-type-requirement.localization (&optional simplifyp)
  (if (and simplifyp (null .sublocation)) .species-type
    [localization .species-type .sublocation]))

(defun create-reaction-type-requirements (rtype side se)
  "Given a reaction-type and a sum-expression appropriate for the lhs or rhs of a species-type, returns a list of reaction-type-requirement objects"
  (cond
   (se (with-fields se
         (.map-terms (lambda (var stoich)
                       (let+ (((stype localization) 
                               (etypecase var
                                 (species-type              (values var nil))
                                 (localization              (values var.entity var.location)))))
                         [reaction-type-requirement rtype side 
                                                    stype localization stoich])))))
   (t  [reaction-type-requirement rtype side nil nil nil]
       nil)))

(defun determine-reaction-type-location-class (defined-lclass lhs rhs)
  (flet ((find-location-class-in-sum-expression (se)
            (mutils:ifit (find-if #'species-type-p (if se se.vars))
                         it.location-class)))
    (let* ((lclass (or defined-lclass 
                       (find-location-class-in-sum-expression lhs)
                       (find-location-class-in-sum-expression rhs))))
      (check-reaction-type-argument lhs lclass)
      (check-reaction-type-argument rhs lclass)
      lclass)))

(defun check-reaction-type-argument (rt-arg loc-class)
  (labels ((coef-ok (num)
             (and (integerp num) (> num 0)))
           (check-term (var num)
             (typecase var
               (species-type         (if (null (subtypep var.location-class loc-class))
                                      (b-error "~S.location-class does not match ~S." var loc-class)))
               (localization         (if (null var.(is-valid-for loc-class))
                                         (b-error "Invalid reaction-type argument: ~S.  No ~S sublocation in location class ~S." var  var.location loc-class)))
               (t            (b-error "Expecting a species-type or location-requirement, but received ~S." var)))
             (unless (coef-ok num)
               (b-error "Invalid stoichiometry (~S) in reaction-type sum-expression ~S" num rt-arg))))
    (when rt-arg 
      (unless (sum-expression-p rt-arg)
        (b-error "Invalid argument to reaction-type: ~S.  Expecting a {na A + nb B + nb C....}" rt-arg))
      rt-arg.(map-terms #'check-term))))

(hide-classes reaction-type-requirement |REACTION-TYPE.LHS-REQUIREMENTS| |REACTION-TYPE.RHS-REQUIREMENTS|)


(defoperator -> ((+ 2 (operator-precedence '+)) :xfy)
  (lhs rhs)
  (if (localization-p lhs)
      [reaction-type lhs.entity rhs lhs.location-class]
    [reaction-type lhs rhs]))

(defoperator <- ((+ 2 (operator-precedence '+)) :xfy)
  (rhs lhs)
  (if (localization-p lhs)
      [reaction-type lhs.entity rhs lhs.location-class]
    [reaction-type lhs rhs]))


;;;; ;;;
;;;; ;;;
;;;; ;;;  REVERSIBLE-REACTION:
;;;; ;;;
(defcon reversible-reaction ()
  ((lhs reaction-type)
   (rhs reaction-type) &optional (location-class := nil)
   &property (fwd reaction-type :#= [reaction-type .lhs .rhs .location-class])
             (rev reaction-type :#= [reaction-type .rhs .lhs .location-class]))
  (unless (math-expression-p lhs) {.lhs := {lhs}})
  (unless (math-expression-p rhs) {.rhs := {rhs}})
  {.location-class := (determine-reaction-type-location-class .location-class .lhs .rhs)})


(defield reversible-reaction.kf ()
  .fwd.k)

(defield (setf reversible-reaction.kf) (value)
  (setf .fwd.k value))

(defield reversible-reaction.kr ()
  .rev.k)

(defield (setf reversible-reaction.kr) (value)
  (setf .rev.k value))

(defoperator <-> ((+ 2 (operator-precedence '+)) :xfy)
  (lhs rhs)
  (if (localization-p lhs)
      [reversible-reaction lhs.entity rhs lhs.location-class]
    [reversible-reaction lhs rhs]))


(defmethod print-concept ((rt reversible-reaction) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression rt stream)))

(defmethod print-math-expression ((rr reversible-reaction) &optional 
                                  (stream *standard-output*) (outer-op t))
  (with-print-context rr
    (let ((lhs-printable (if (find-if #'species-type-p rr.lhs.vars) rr.lhs
                           [localization rr.lhs rr.location-class])))
      (print-operator '<-> (list lhs-printable rr.rhs) stream outer-op))))



;;;; (defield reaction-type.lhs-species (loc req)
;;;;   (get-reaction-type-species object loc req .reactants))

;;;; (defield (setf reaction-type.lhs-species) (value loc selector)
;;;;   (setf (get-reaction-type-species object loc selector .reactants) value))

;;;; (defield reaction-type.rhs-species (loc selector)
;;;;   (get-reaction-type-species object loc selector .products))

;;;; (defield (setf reaction-type.rhs-species) (value loc req)
;;;;   (setf (get-reaction-type-species object loc req .products) value))

;;;; (defun get-reaction-type-species (rt loc selector ht)
;;;;   (let* ((key (etypecase selector
;;;;                 (reaction-type-requirement nil)
;;;;                 (location-requirement      ?.location-requirement)
;;;;                 (species-type              ?.species-type)))
;;;;          (retval (assoc (gethash loc ht) selector :key key)))
;;;;     (unless retval (b-error "Invalid selector (~S). No matching species in ~S." selector rt))))

;;;; (defun (setf get-reaction-type-species) (rct rt loc req ht)
;;;;   (assert (and (species-p rct) (eq req.reaction-type rt)))
;;;;   (pushnew (cons req rct) (gethash loc ht) :key #'car))

;;;; (defield reaction-type.satisfied-at (loc)
;;;;   (eq (length (gethash loc .reactants)) 
;;;;       (length .lhs-requirements)))

