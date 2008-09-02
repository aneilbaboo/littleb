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

;;; File: reaction-type.lisp
;;; Description:  

;;; $Id: reaction-type.lisp,v 1.14 2008/09/02 14:58:10 amallavarapu Exp $

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
  (multiple-value-bind (lhs rhs loc-class)
      (parse-reaction-type-arguments lhs rhs location-class '-> #'canonicalize-species-type)
    (setf .lhs (apply #'s+ lhs)
          .rhs (apply #'s+ rhs)
          .location-class loc-class))
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
                           {rt.lhs _@ rt.location-class})))
      (pprint-math-form `{,lhs-printable -> ,rt.rhs} stream outer-op))))


(defield species-type.required (subloc)
  "Returns an object which represents a requirement for a species in a relative location.  SUBLOC is a keyword which names a field of a location which points to another location."
  (b-warn "~S.(REQUIRED ~S) is deprecated: use ~0@*{~S @ ~S}" object subloc)
  {object @ subloc})
;;;
;;; reaction-type-requirement - 
;;;
(defcon reaction-type-requirement (:notrace)
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

;;;;
;;;; PARSING OF REACTION-TYPE ARGUMENTS:
;;;;
(defun canonicalize-reaction-type-argument (x type-fn)
  "Returns:
     * the canonicalized object
     * any sublocations referenced
     * location-classes
   TYPE-FN is a function which is called when an object is expected to represent
   an object being transformed by the reaction.  The job of this function is to return
   the canonical form of the object refered to."
  (etypecase x
    (sum-expression  (canonicalize-reaction-type-argument x.as-list type-fn))
    (list            (loop for elt in x
                           for (cx subloc loc-class) = 
                           (multiple-value-list (canonicalize-reaction-type-argument elt type-fn))
                           append cx into cxes
                           append subloc into sublocs
                           append loc-class into loc-classes
                           finally (return (values cxes 
                                                   (remove-duplicates sublocs)
                                                   (remove-duplicates loc-classes)))))
    (localization    (cond
                      ((location-class-p x.location)
                       (multiple-value-bind (cx sublocs loc-classes)
                           (canonicalize-reaction-type-argument x.entity type-fn)
                         (values cx
                                 sublocs
                                 (remove-duplicates (list* x.location loc-classes)))))
                      (t (multiple-value-bind (cx sublocs loc-classes)
                             (canonicalize-reaction-type-argument x.entity type-fn)
                           (declare (ignore loc-classes))
                           (values (list [localization (first cx) x.location])
                                   (list* x.location sublocs)
                                   nil)))))
    (t               (let ((o (funcall type-fn x)))
                       (values (list o) nil (list o.location-class))))))
                             

(defun parse-reaction-type-arguments (lhs rhs location-class op type-fn)
  (mutils:let+ (((clhs lsublocs lloc-classes) (canonicalize-reaction-type-argument lhs type-fn))
                ((crhs rsublocs rloc-classes) (canonicalize-reaction-type-argument rhs type-fn))
                (loc-classes                  (remove-duplicates (nconc lloc-classes rloc-classes
                                                                        (if location-class 
                                                                            (list location-class)))))
                (sublocs                      (remove-duplicates (nconc lsublocs rsublocs)))
                (loc-class                    (first loc-classes)))
    (cond
     ((> (length loc-classes) 1)
      (b-error "Unable to determine location class for {~{~S~^ + ~} ~A ~{~S~^ + ~}} ~
                ~&-- have you forgotten to assign a sublocation to one of the monomer (e.g., {[some-monomer...] @ :inner})?"
               clhs op crhs))
     ((null loc-classes)
      (b-error "Unable to determine location class for {~{~S~^ + ~} ~A ~{~S~^ + ~}}."
               clhs op crhs))
     ((remove-if #'null
                 (set-difference 
                  sublocs 
                  (mapcar #'fieldinfo-symbol
                          (location-class-sublocations loc-class))))
      (b-error "Invalid sublocation for ~S: ~{~S~^, ~}."
               loc-class (set-difference sublocs (location-class-sublocations loc-class)))))
    (values clhs crhs loc-class)))

(defun canonicalize-species-type (x)
  (unless (species-type-p x)
    (b-error "Expecting a species-type, but received ~S." x))
  x)

;;;
;;; END ARGUMENT PARSING
;;;

(defoperator -> ((+ 2 (operator-precedence '+)) :xfy)
  (lhs rhs)
  [reaction-type lhs rhs])

;;;;   (if (localization-p lhs)
;;;;       [reaction-type lhs.entity rhs lhs.location-class]
;;;;     [reaction-type lhs rhs]))

(defoperator <- ((+ 2 (operator-precedence '+)) :xfy)
  (rhs lhs)
  [reaction-type lhs rhs])

;;;;   (if (localization-p lhs)
;;;;       [reaction-type lhs.entity rhs lhs.location-class]
;;;;     [reaction-type lhs rhs]))


;;;; ;;;
;;;; ;;;
;;;; ;;;  REVERSIBLE-REACTION:
;;;; ;;;
(defcon reversible-reaction ()
  ((lhs reaction-type)
   (rhs reaction-type) &optional (location-class := nil)
   &property (fwd reaction-type :#= [reaction-type .lhs .rhs .location-class])
             (rev reaction-type :#= [reaction-type .rhs .lhs .location-class]))
  (multiple-value-bind (clhs crhs loc-class)
      (parse-reaction-type-arguments lhs rhs location-class '<-> #'canonicalize-species-type)
   (setf .lhs (apply #'s+ clhs)
         .rhs (apply #'s+ crhs)
         .location-class loc-class)))


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


;;;;
;;;; THE OLD WAY:
;;;;
(defield reaction-type.satisfied-at (loc)
  (eq (length (gethash loc .reactants)) 
      (length .lhs-requirements)))

(defield reaction-type.lhs-species (loc req)
  (get-reaction-type-species object loc req .reactants))

(defield (setf reaction-type.lhs-species) (value loc selector)
  (setf (get-reaction-type-species object loc selector .reactants) value))

(defield reaction-type.rhs-species (loc selector)
  (get-reaction-type-species object loc selector .products))

(defield (setf reaction-type.rhs-species) (value loc req)
  (setf (get-reaction-type-species object loc req .products) value))

(defun get-reaction-type-species (rt loc selector ht)
  (let* ((key (etypecase selector
                (reaction-type-requirement nil)
                (localization              (lambda (rt)
                                             [localization rt.species-type 
                                                           rt.sublocation]))
                (species-type              ?.species-type)))
         (retval (cdr (assoc selector (gethash loc ht) :key key))))
    (or retval
        (b-error "Invalid selector (~S). No matching species in ~S." selector rt))))

(defun (setf get-reaction-type-species) (rct rt loc req ht)
  (assert (and (species-p rct) (eq req.reaction-type rt)))
  (pushnew (cons req rct) (gethash loc ht) :key #'car))




;;;; (defun determine-reaction-type-location-class (defined-lclass lhs rhs)
;;;;   (flet ((find-location-class-in-sum-expression (se)
;;;;             (mutils:ifit (find-if #'species-type-p (if se se.vars))
;;;;                          it.location-class)))
;;;;     (let* ((lclass (or defined-lclass 
;;;;                        (find-location-class-in-sum-expression lhs)
;;;;                        (find-location-class-in-sum-expression rhs))))
;;;;       (check-reaction-type-argument lhs lclass)
;;;;       (check-reaction-type-argument rhs lclass)
;;;;       lclass)))

;;;; (defun check-reaction-type-argument (rt-arg loc-class)
;;;;   (labels ((coef-ok (num)
;;;;              (and (integerp num) (> num 0)))
;;;;            (check-term (var num)
;;;;              (typecase var
;;;;                (species-type         (if (null (subtypep var.location-class loc-class))
;;;;                                       (b-error "~S.location-class does not match ~S." var loc-class)))
;;;;                (localization         (if (null var.(is-valid-for loc-class))
;;;;                                          (b-error "Invalid reaction-type argument: ~S.  No ~S sublocation in location class ~S." var  var.location loc-class)))
;;;;                (t            (b-error "Expecting a species-type or location-requirement, but received ~S." var)))
;;;;              (unless (coef-ok num)
;;;;                (b-error "Invalid stoichiometry (~S) in reaction-type sum-expression ~S" num rt-arg))))
;;;;     (when rt-arg 
;;;;       (unless (sum-expression-p rt-arg)
;;;;         (b-error "Invalid argument to reaction-type: ~S.  Expecting a {na A + nb B + nb C....}" rt-arg))
;;;;       rt-arg.(map-terms #'check-term))))
