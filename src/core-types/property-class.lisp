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

;;; File: property-class
;;; Description: defines the metaclass of property.

;;; $Id: property-class.lisp,v 1.1 2007/09/25 17:54:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;;
;;; PROPERTY-CLASS
;;;

(defclass property-class (kb-class) 
  ((field-symbol       :reader pclass-field-symbol)
   (parent-cclass       :reader pclass-parent-cclass) ; the cclass on 
   (default-value-ctor :reader pclass-default-value-ctor)
   (value-ctor         :reader pclass-value-ctor)
   (type               :reader pclass-type :initform nil)
   (default-relevance  :reader pclass-default-relevance)
   ; the instance used when :allocation is :class
   (static-instance    :accessor pclass-static-instance))) 

(defmethod fld ((object property-class) (f (eql :field-symbol)) &rest args)
  (pclass-field-symbol object))
(defmethod fld ((object property-class) (f (eql :parent-class)) &rest args)
  (pclass-parent-cclass object))
(defmethod fld ((object property-class) (f (eql :type)) &rest args)
  (pclass-type object))
(defmethod fld ((object property-class) (f (eql :default-relevance)) &rest args)
  (pclass-default-relevance object))

(define-global-const property-class (find-class 'property-class))

(defun pclass-has-default-value (pclass)
  (if (pclass-default-value-ctor pclass) t))

(defun pclass-construct-value (pclass obj input-val)
  (ifit (pclass-value-ctor pclass)
      (funcall it obj input-val) input-val))

(defun pclass-create-instance (pclass obj val)
  (let* ((hashkey (pclass-hashkey pclass obj))
         (new     (make-instance pclass :object obj 
                                 :value val
                                 :relevance (or *relevance* (pclass-default-relevance pclass)))))
    (kb-add-object hashkey new)
    new))

(defun pclassp (o)
  (typep o 'property-class))


(defun pclass-hashkey (pclass obj)
  (list (class-name pclass) obj))



;;;
;;; PRINT-OBJECT PCLASS method
;;;
(defmethod print-object ((o property-class) stream)
  (if *print-context*
      (prin1 (class-name o) stream)
    (print-unreadable-object (o stream :type t)
      (prin1 (class-name o) stream))))


)
