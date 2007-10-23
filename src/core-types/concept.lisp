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

;;; File: concept
;;; Description: defines the base class, CONCEPT

;;; $Id: concept.lisp,v 1.2 2007/10/23 17:16:49 amallavarapu Exp $
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
  (concept-doc object))
(defmethod (setf fld)  (value (object concept) (field (eql :documentation)) &rest args)
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
  (ifit (fld o :_doc) (format stream "~A~%" it))
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


