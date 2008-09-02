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

;;; File: defprop
;;; Description: defprop, the definer of properties
;;;

;;; $Id: defprop.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $
;;;

(in-package b)

(portable:define-dspec-class defprop nil nil)

(define-field-form-parser defprop)

(defmacro defprop (prop-name
                   (&rest options) 
                   &body body) 
  (handler-case 
      (progn 
        (cond
         ((not (fld-form-p prop-name))
          (error "Expecting (DEFPROP CLASS.PROP-NAME...), but received (DEFPROP ~S...)" prop-name))
         ((not (global-const-fld-form-p prop-name))
          (error "~S must be a concept-class." (fld-form-object prop-name))))

        (let+ ((cclass-symbol             (fld-form-object prop-name))
               (cclass                    (find-class cclass-symbol))
               (class-base-property       (cclass-base-property cclass))
               (class-base-prop-symbol    (class-name class-base-property))
               (field-symbol              (fld-form-field prop-name))
               (parent-prop               (ifit (cclass-property cclass field-symbol nil)
                                                          (if (eq cclass-symbol (pclass-parent-cclass it)) 
                                                              (pclass-parent-cclass it) 
                                                            class-base-prop-symbol)
                                                        class-base-prop-symbol))
               (prop-symbol               (delim-sym #\. cclass-symbol field-symbol))
               ((type setfval defval
                 relevance ctor allocation
                 doc)                     (read-property-options options)))
          (declare (special current-property)) ; for use by with-fields
          ; no longer (check-field-symbol field-symbol)

          (if (and (not body) (not (missingp ctor)))
              (setf body (list ctor)))
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (portable:dspec (defprop ,prop-symbol)
               (portable:record-definition `(defprop ,',prop-symbol) (portable:location))

               (process-name-exposure ',prop-symbol :expose)

               (defclass ,prop-symbol (,parent-prop)
                 ((object) (value ,@(if (and (symbolp type) (find-class type nil)) `(:type ,type))
                                  :allocation ,allocation))
                 (:metaclass property-class)
                 ,@(if doc `((:documentation ,doc))))

               (define-global-const ,prop-symbol (find-class ',prop-symbol))

               (initialize-pclass-object ,prop-symbol ,cclass-symbol ',field-symbol
                                         ',prop-symbol
                                         ',type 
                                         ,(make-default-value-lambda field-symbol setfval defval)
                                         ',relevance)

               ,@(funcall (if (eq allocation :class)
                              'build-static-property-field-definitions 
                            'build-property-field-definitions)
                          prop-name prop-symbol type doc body)

               ',prop-symbol))))
  (error (e) (error "Invalid property ~A: ~A" prop-name e))))

;;;; (defun prop-val-ctor-name (prop-symbol)
;;;;   (hidden-symbol prop-symbol 'ctor))

(defun build-property-field-definitions (prop-name prop-symbol type doc body)
  (let ((val        (gensym "VAL"))
        (existing   (gensym "EXISTING"))
        (prop-field (fld-form-field prop-name))
        (doclist    (if doc (list doc))))
    `(;; the setf field:
      (define-field (setf ,prop-name) (,type &property t (pclass-update-fn ,prop-symbol))
        (value)
        ,@doclist
        (kb-transaction
          (let ((,val ,(if body `(progn ,@body) 'value))
                (,existing (concept-property object ',prop-field)))
            ,@(when type `((check-property-value-type ,val ',prop-field ',type)))
            (cond 
             (,existing (property-update ,existing ,val))
             
             (t         (set-new-concept-property object 
                                                  (pclass-create-instance ,prop-symbol object ,val))))
            ,val)))
      ;; the get field:
      (define-field ,prop-name (,type &property t) ()
        ,@doclist
        (concept-property-value object ',prop-field)))))

;;;; (defun build-static-property-field-definitions (prop-name prop-symbol type doc body)
;;;;   "Field definitions when a property's allocation is :CLASS"
;;;;    (let ((val        (gensym "VAL"))
;;;;          (existing   (gensym "EXISTING"))
;;;;          (prop-field (fld-form-field prop-name))
;;;;          (doclist    (if doc (list doc))))
;;;;     `(;; the setf field:
;;;;       (define-field (setf ,prop-name) (,type &property t (pclass-update-fn ,prop-symbol))
;;;;         (value)
;;;;         ,@doclist
;;;;         (let ((,val value))
;;;;           (setf (property-value (make-instance ',prop-symbol)) ,val)))
;;;;       ;; the get field:
;;;;       (define-field ,prop-name (,type &property t) ()
;;;;         ,@doclist
;;;;         (property-value (make-instance ',prop-symbol))))))


(defun pclass-update-fn (pclass)
  (lambda (c) (setf (cclass-property c) pclass)))

;;;; (defun build-property-val-ctor (cclass prop-symbol prop-field type body)
;;;;   (let ((prop-var   (intern (symbol-name prop-field)))
;;;;         (val        (gensym "PROPVAL")))
;;;;     `(defun ,prop-symbol (object value)
;;;;        (with-fields object
;;;;          (let ((,val   ,(if body `(progn ,@body) 'value)))
;;;;            ,@(when type `((check-property-value-type ,val ',prop-field ',type)))
;;;;            ,val)))))

(defun check-property-value-type (val field type)
    (if (not (xtypep val type))
        (error "Invalid type.  Attempt to assign ~S (type ~S) to property ~A (type ~S)" 
               val (type-of val) field type)))

(defun initialize-pclass-object (pclass cclass new-field-symbol new-val-ctor 
                                        new-type default-value-fn new-default-rel)
  (with-slots (field-symbol value-ctor type default-value-ctor default-relevance parent-cclass) pclass
    (setf parent-cclass         cclass
          field-symbol          new-field-symbol
          value-ctor            new-val-ctor
          type                  new-type
          default-value-ctor    default-value-fn
          default-relevance     new-default-rel))
  (kb-class-init pclass)
  (cclass-add-property cclass pclass))

(defun make-default-value-lambda (field setfval defval)
  (cond 
   ((and (not (missingp setfval))
         (not (missingp defval)))
    (error "Both := and :#= property options were provided. (:=  ~S, :#= ~S)"
           setfval defval))

   ((not (missingp setfval))
    `(lambda (object) 
       (with-fields object
         (setf (FLD object ',field) ,setfval))))

   ((not (missingp defval))
    `(lambda (object)
       (with-fields object
         (setf-name (FLD object ',field) ,defval))))

   (t                    nil)))

(defun property-accessor-forms (cclass field)
  (let ((fld-accessor  (field-accessor-fn-name (class-name cclass) field))
        (obj           (gensym "OBJ"))
        (val           (gensym "VAL")))
    `((defun ,fld-accessor (,obj) (concept-property-value ,obj ,field))
      (defun (setf ,fld-accessor) (,val ,obj) 
        (setf (concept-property-value ,obj ,field) ,val)))))

(defun read-property-options (options)
  (let+ ((first-opt     (first options))
         (type          (if (not (keywordp first-opt)) first-opt))
         (rest-opt      (if type (rest options) options))
         (relevance     (getf rest-opt :relevance))
         (ctor          (getf rest-opt :ctor ?))
         (setfval       (getf rest-opt := ?))
         (defval        (getf rest-opt :#= ?))
         (doc           (getf rest-opt :documentation nil))
         (allocation    (getf rest-opt :allocation :instance)))
    (check-plist rest-opt '(:relevance :ctor := :#= :allocation :documentation))
    (when (and (not (missingp setfval))
               (not (missingp defval)))
      (error "Both := and :#= property options were provided. (:=  ~S, :#= ~S)"
             setfval defval))
    
    (values type setfval defval relevance ctor allocation doc)))

(defun make-property-class-symbol (cclass-symbol property-symbol)
  (delim-sym #\. cclass-symbol property-symbol))

(port:setup-indent 'defprop 2 2 4)
