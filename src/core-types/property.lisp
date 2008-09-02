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

;;; File: property
;;; Description: Defines property objects

;;; $Id: property.lisp,v 1.3 2008/09/02 14:58:10 amallavarapu Exp $
;;;

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; introduce field symbols for the property class
;;;
;;; PROPERTY
;;;
(defclass property () 
  ((object :initarg :object :accessor property-object)
   (value :initarg :value :accessor property-value)
   (relevance :initarg :relevance :accessor property-relevance))
  (:metaclass property-class))

(define-global-const property (find-class 'property))

(setf (slot-value concept 'base-property) property)

(defmethod property ((o property))
  (property-value o))
  
(defun relevance (obj field)
  (property-relevance (concept-property obj field)))

(defun property-hashkey (prop)
  (list (class-name (class-of prop)) (property-object prop)))

(defun propertyp (o)
  (typep o 'property))

(defun prop-object-form-p (o)
  (and (object-form-p o)
       (keywordp (object-form-object o))))

(defun relevance-conflict (relevance existing-relevance)
  (and (eq relevance t) (eq existing-relevance t)))

(defun relevance> (relevance existing-relevance &key (nil-policy :change)) ;; relevance comparator
  (or (and (or relevance (eq nil-policy :change))
               (null existing-relevance))
      (member relevance '(:force t))
      (and (eq relevance t))
      (and (numberp relevance) (numberp existing-relevance)
           (> relevance existing-relevance))))

(defun normalize-relevance (r prev-rel) 
  (cond
    ((or (null r)  (integerp r) (eq r t))   r)
    ((eq r :force) prev-rel)
    (t   (error "Unexpected relevance value: ~S" r))))


(defun property-update (p val)
  (kb-transaction
    (symbol-macrolet ((pclass (Class-of p)))
      (let* ((obj     (property-object p))
             (curval  (property-value p))
             (currel  (property-relevance p)))
        (cond
         ((relevance-conflict *relevance* currel) 
          (if (not (equalp curval val))
              (error "Relevance conflict: both current and new relevance ~
                                are T when updating property ~A of ~A" 
                     (pclass-field-symbol pclass) obj)
            val))
        
         ((relevance> *relevance* currel)

          (setf (property-relevance p) (normalize-relevance *relevance* currel))

          (when (not (equalp val curval))
            (setf (property-value p) val)
            (kb-update-object p)
            val))
         ((eq curval val) curval)
         (t
          (b-warn "Failed to set ~S.~A to ~S (relevance ~S).  ~&Keeping current value ~S (relevance ~S).~&" 
                obj (pclass-field-symbol pclass) val *relevance*
                curval currel)
          curval))))))


;;;
;;; PRINT-OBJECT PROPERTY method
;;;
(defmethod print-object ((o property) stream)
  (format stream "#<~A.~A ~A>" 
          (property-object o)
          (pclass-field-symbol (class-of o))
          (property-value o)
          (property-relevance o)))

)

