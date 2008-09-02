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

;;; File: concept
;;; Description: defines the base class, CONCEPT

;;; $Id: concept.lisp,v 1.5 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
;;;
;;; CONCEPT
;;;
(defclass concept ()
  ((properties   :initform (make-hash-table) 
                 :reader concept-properties)
   (interfaces   :initform () ;; a property list
                 :accessor concept-interfaces)
   (doc          :initform () 
                 :accessor concept-doc)
   (hashkey      :accessor concept-hashkey
                 :initform ()))
  (:metaclass concept-class))

(defmethod kb-object-hashkey ((c concept)) (concept-hashkey c))
(defmethod (setf kb-object-hashkey) (hk (c concept)) (setf (concept-hashkey c) hk))

(defmethod documentation ((object concept) (doc-type t))
  (concept-doc object))

;;; documentation methods
(defmethod documentation ((object concept) (doc-type (eql :ctor-help)))
  (let ((*print-context* '(t)))
    (format nil "INITIALIZER: [~S ...] ~@[FIELDS: ~{~A~^, ~}~]~@[ -- ~A~]"
            object
            (mapcar #'fieldinfo-symbol 
                    (remove :id (class-all-fieldinfos (class-of object))
                            :key #'fieldinfo-kind))
            (concept-doc object))))

(defmethod documentation ((object concept) (doc-type (eql :fields-help)))
  (documentation (class-of object) doc-type))

(defmethod documentation ((object concept) (doc-type cons))
  (documentation (class-of object) doc-type))

(defmethod fld  ((object concept) (field (eql :documentation)) &rest args)
  (declare (ignore args))
  (concept-doc object))
(defmethod (setf fld)  (value (object concept) (field (eql :documentation)) &rest args)
  (declare (ignore args))
  (setf (concept-doc object) value))

(define-global-const concept (the concept-class (find-class 'concept)))

(defun conceptp (o)
  (typep o 'concept))

(defconstant +invalid-field+ (make-symbol "INVALID-FIELD"))

(defun concept-property (o field)
  (gethash field (concept-properties o)))
      
(defun set-new-concept-property (o prop)
  (assert (propertyp prop) () "First arg to (setf concept-property) should be a property")
  (setf (gethash (pclass-field-symbol (class-of prop)) (concept-properties o)) prop))

(defun concept-property-value (o field &optional (errorp t))
  (let ((prop (concept-property o field)))
    (cond 
     (prop    (property-value prop))
     (errorp  (let ((pclass (cclass-property (class-of o) field)))
                (if (pclass-has-default-value pclass)
                    (funcall (pclass-default-value-ctor pclass) o)
                  (unbound-property-error o field))))
     (t       ?))))


(defun concept-initialize (c initializer)
  (when initializer (funcall initializer c))
  c)

(defun concept-property-bound-p (o field)
  "Returns T or NIL indicating whether the property has been defined"
  (cclass-property (class-of o) field) ; throws an error if FIELD is not a valid property
  (if (concept-property o field) t))
  
(defun unbound-property-error (obj field)
  (b-error "Property ~S of ~S is unbound." field obj))

(defun concept-interface (c iclass)
  (getf (concept-interfaces c) iclass))

(defsetf concept-interface (c iclass) (iobj)
  `(setf (getf (concept-interfaces ,c) ,iclass) ,iobj))

(declaim (inline concept-temporary-p))
(defun concept-temporary-p (c) (unless (concept-hashkey c) nil))

(defun init-instance (o initializer)
  (when initializer (funcall initializer o))
  o)

;;;
;;; Describe object method
;;;

(defmethod describe-object ((o concept) stream)
  (ignore-errors (ifit (fld o :documentation) (format stream "~A~%" it)))
  (prin1 o stream)
  (format stream "~%~%ID-FIELDS:~%")
  (loop for fi in (fieldinfos o)
        when (eq (fieldinfo-kind fi) :id)
        do (format stream "~30A~S~%" (fieldinfo-symbol fi)
                                   (fld o (fieldinfo-symbol fi))))
  (format stream "~%PROPERTY-FIELDS:~%")
  (loop for pclass in (cclass-properties (class-of o))
        for symbol = (pclass-field-symbol pclass)
        for prop = (concept-property o symbol)
        do (format stream "~30A~S~%" (pclass-field-symbol pclass)
                   (if prop (property-value prop) '?))))
  


)


