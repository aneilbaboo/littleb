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


;;; File: property
;;; Description: Defines property objects

;;; $Id: property.lisp,v 1.4 2008/09/06 00:23:08 amallavarapu Exp $
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

