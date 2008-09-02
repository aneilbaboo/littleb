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

;;; File: property-class
;;; Description: defines the metaclass of property.

;;; $Id: property-class.lisp,v 1.3 2008/09/02 14:58:10 amallavarapu Exp $
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
  (declare (ignore args))
  (pclass-field-symbol object))
(defmethod fld ((object property-class) (f (eql :parent-class)) &rest args)
  (declare (ignore args))
  (pclass-parent-cclass object))
(defmethod fld ((object property-class) (f (eql :type)) &rest args)
  (declare (ignore args))
  (pclass-type object))
(defmethod fld ((object property-class) (f (eql :default-relevance)) &rest args)
  (declare (ignore args))
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
