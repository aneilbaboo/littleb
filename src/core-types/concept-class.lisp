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

;;; File: concept-class
;;; Description: Defines the meta-class CONCEPT-CLASS. 

;;; $Id: concept-class.lisp,v 1.8 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)

;(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass kb-class (standard-class)
  ((matchable-p :reader kb-class-matchable-p :initform nil)
   (unmatched-instances :accessor kb-class-unmatched-instances :initform nil)
   (traceable-p :accessor kb-class-traceable-p :initform nil)))

(defgeneric kb-object-hashkey (object)
  (:method (object) (declare (ignorable object)) nil))

(defgeneric (setf kb-object-hashkey) (key object)
  (:method (key object) (declare (ignorable key object)) nil))

#+allegro
(defmethod make-load-form ((o kb-class) &optional env)
  `(find-class ',(class-name o)))

(defun kb-class-search-for-matchable-p (x)
  (typecase x
    (kb-class      (or (kb-class-matchable-p x)
                       (kb-class-search-for-matchable-p (portable:class-direct-superclasses x))))
    (symbol        (kb-class-search-for-matchable-p (find-class x)))
    (cons          (some #'kb-class-search-for-matchable-p x))))

(defun kb-class-init (x)
  (when (kb-class-search-for-matchable-p x)
    (kb-register-matchable-class x)))
;;;
;;; CONCEPT-CLASS
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass concept-class (kb-class)  
  ((ctor-args-fn       :accessor cclass-ctor-args-fn :initform nil)
   (pattern-args-fn    :accessor cclass-pattern-args-fn :initform nil)
   (id-field-order     :accessor cclass-id-field-order :initform nil) ; the id-fields
   (properties         :accessor cclass-properties :initform ()); ordered list of property classes
   (abstract-p         :initform nil :reader cclass-abstract-p)
   (exposure           :initform :expose :reader cclass-exposure)
   (matchable-p        :initform nil :reader cclass-matchable-p)
   (where-patterns     :initform nil 
                       :reader cclass-where-patterns) ; key = :KEY val = property-class
   (base-property      :initform nil
                       :reader cclass-base-property))))


(allowing-redefinitions
(defmethod documentation (x (doc-type (eql :ctor-help))) ())
(defmethod documentation (x (doc-type (eql :fields-help))) ())
(defmethod documentation (x (doc-type cons)) ()))
;;;
;;; documentation methods used by help system
;;;
(defmethod documentation ((cclass concept-class) (doc-type (eql :ctor-help)))
  (format nil "CTOR: [~A~{ ~A~}]~@[ FIELDS: ~{~A~^, ~}~]~@[ -- ~A~]"
          (class-name cclass)
          (cclass-ctor-lambda-list cclass)
          (set-difference (mapcar #'b::fieldinfo-symbol (b::class-all-fieldinfos cclass))
                          (cclass-id-field-order cclass))
          (documentation cclass t)))

(defmethod documentation ((cclass concept-class) (doc-type (eql :fields-help)))
  (format nil "~A FIELDS: ~{~A~^, ~}"
          (class-name cclass)
          (mapcar #'fieldinfo-symbol (class-all-fieldinfos cclass))))

(defmethod documentation ((class concept-class) (doc-type cons))
  (ecase (first doc-type)
    (:field-help
     (let ((field (cdr doc-type)))
       (let* ((class-name (class-name class))
              (fi         (when class (class-fieldinfo class field)))
              (args       (if fi (fieldinfo-args fi)))
              (method     (when class 
                            (or (find-method #'fld () `(,class (eql ,field)) nil)
                                (first (compute-applicable-methods 
                                        #'fld 
                                        (list (clos:class-prototype class) field nil))))))
              (doc        (when method (documentation method t))))
         (when fi
           (if args
               ;; fields with arguments formatted FIELDINFO-KIND: CLASS.(FIELD PARAMS) -- DOC
               (format nil "~A: ~A.(~A~{ ~A~})~@[ -- ~A~]" 
                        (fieldinfo-kind fi) class-name field args doc)
             ;; fields without args have this FIELDINFO-KIND: CLASS.FIELD -- DOC
             (format nil "~A: ~A.~A~@[ -- ~A~]" 
                     (fieldinfo-kind fi) class-name field doc))))))))

(define-global-const concept-class (find-class 'concept-class))

(defmethod fld :around ((o concept-class) (field (eql :_name)) &rest args)
  (declare (ignore args))
  (class-name o))

(defmethod fld ((o concept-class) (field (eql :_abstract)) &rest args)
  (declare (ignore args))
  (cclass-abstract-p o))

(defmethod fld ((o concept-class) (field (eql :_exposure)) &rest args)
  (declare (ignore args))
  (cclass-exposure o))

(defmethod fld ((o concept-class) (field (eql :_properties)) &rest args)
  (declare (ignore args))
  (cclass-properties o))

;;;
;;; lambda list
;;; 
(defun cclass-ctor-lambda-list (cclass)
  (let ((cclass (if (symbolp cclass) (find-class cclass) cclass)))
    (loop for fld in (cclass-id-field-order cclass)
          for last-switch = nil then cur-switch
          for cur-switch = (fieldinfo-lambda-switch (class-fieldinfo cclass fld))
          unless (eq cur-switch last-switch) 
          collect cur-switch
          collect fld)))
        

;;;
;;; ID FIELD ADD/GET
;;;
(defun cclass-add-id-field (cclass idinfo)
  (add-fieldinfo cclass idinfo nil))

;;;
;;; PROPERTY ADD/GET
;;;
(defun cclass-add-property (cclass pclass)
  (let* ((fsym     (pclass-field-symbol pclass))
         (fi       (make-fieldinfo cclass  ; the defclass
                                   fsym   ; symbol
                                   nil    ; no args
                                   (pclass-type pclass) ;; type
                                   t      ; matchable
                                   nil    ; default not recorded here
                                   '&property))
         (updatefn (lambda (c) (setf (cclass-property c) pclass))))

    (add-fieldinfo cclass fi updatefn)))

(defun cclass-property (cclass field &optional (errorp t))
  (let ((pclass (find field (cclass-properties cclass) :key #'pclass-field-symbol)))
    (cond
     (pclass pclass)
     (errorp (error "Property ~A is not defined in ~S" field (class-name cclass))))))

;;;; (defun check-x-props ()
;;;;   (let ((xclass (find-class 'b-user::x nil)))
;;;;     (when xclass
;;;;       (format t "~&X.PROPERTIES = ~S~%" 
;;;;               (cclass-properties xclass)))))
(defun (setf cclass-property) (pclass cclass)
  ;; (format t "~&BEGIN (SETF (CCLASS-PROPERTY ~S) ~S)~%" cclass pclass)
  ;; (check-x-props)
  (let* ((props    (cclass-properties cclass))
         (existing (find (pclass-field-symbol pclass) props :key #'pclass-field-symbol)))
    (if existing 
        (setf (cclass-properties cclass) (nsubstitute pclass existing props))
      (pushend pclass (cclass-properties cclass)))))
      ;; (format t "~&END (SETF (CCLASS-PROPERTY ~S) ~S)~%" cclass pclass)
      ;;(check-x-props))))
       
(defmacro tracing (x)
  (let ((result '#:result))
  `(progn (format t "~&> ~S~%" ',x *standard-output*)
     (let ((,result ,x))
       (format t "~&  => ~S~%"  ,result)
       ,result))))

(defun cclass-init-from-superclasses (cclass)
  (loop for super in (portable:class-direct-superclasses cclass)
        when (cclassp super)
        do (with-slots (properties) cclass
             (let* ((existing    properties)
                    (super-props (remove-if (lambda (x) (find x existing 
                                                              :key #'pclass-field-symbol))
                                            (cclass-properties super)
                                            :key #'pclass-field-symbol)))
               
               (setf properties (append existing (copy-list super-props)))))))

(defun cclass-check-id-fieldinfos (cclass)
  "Ensures that the def-class of each id-fieldinfo is the same as cclass"
  (mapcar (lambda (fi)
            (when (and (eq (fieldinfo-kind fi) :ID)
                       (not (eq (fieldinfo-parent fi) cclass)))
              (error "The ID field ~A must be defined in ~S."
                     (fieldinfo-symbol fi)
                     (class-name cclass))))
           (class-fieldinfos cclass)))
    
                
;;;
;;; RESET DEFINITION OF CCLASS
;;;
(defun cclass-reset-definition (cclass)
  (with-slots (ctor-args-fn implementations implementation-ctors pattern-args-fn
                            id-field-order properties abstract-p superclass ; rules - no impl rule anymore
                            implication-fn where-patterns)
      cclass
    (setf ctor-args-fn nil
          implementations nil
          implementation-ctors nil
          pattern-args-fn nil
          id-field-order nil
          properties ()
          abstract-p nil
          superclass nil
          implication-fn nil
          where-patterns nil)))

;;;; (defmethod cclass-read-ctor-args ((cclass symbol) args)
;;;;   (cclass-read-ctor-args (find-class cclass) args))

(defun cclass-read-ctor-args (cclass &rest args)
  "Returns a plist of the form ('_:FIELD1 VAL1 '_:FIELD2 VAL2 ...)"
  (flet ((ctor-error (msg) 
           (error "~A in ~A." msg 
                  `(object ,(class-name cclass) ,@args))))
    (case (first args)

      (:kw ; keyword list mode
       (let (plist)
         (map-plist (lambda (k v)
                      (push (slot-symbol k) plist)
                      (push v plist))
                    (cdr args))
         (reverse plist)))

      (otherwise
       (let ((fn (cclass-ctor-args-fn cclass)))
         (with-context-info ("Reading arguments for ~S: ~S" cclass args)
           (apply fn args)))))))
;;;; (conditions:wrong-num-args-error (e) (ctor-error "Wrong number of arguments"))
;;;; (conditions:bad-arguments-error  (e) (ctor-error "Bad arguments"))
;;;; (error (e) (ctor-error (format nil "~A" e)))))))))

(defmethod cclass-read-pattern-args ((head symbol) args)
  (cclass-read-pattern-args (find-class head) args))

(defmethod cclass-read-pattern-args ((cclass concept-class) args)
  "Returns an assoc list of the form ((:FIELD1 . VAL1) (:FIELD2 . VAL2).  Note: Assocs with values of ? or NIL are removed automatically"
  (handler-case 
      (case (first args) 
        (:kw ; keyword list mode
         (map-plist (lambda (k v)
                      (cons (key k) v))
                    (cdr args)))
        (otherwise 
         (let ((fn (cclass-pattern-args-fn cclass)))
           (apply fn args))))
    (error (e) (b-error "[~{~S~^ ~}] argument list is invalid: ~A." 
                      `(,(class-name cclass) ,@args) e))))

(defun cclassp (o)
  (typep o 'concept-class))

(defun cclass-make-hashkey (cclass id-plist)
  (let ((hk (make-array (1+ (/ (the integer (length id-plist)) 2)))))
    (declare (type simple-vector make-array))
    (setf (svref hk 0) cclass)
    (loop for i = 1 then (1+ i)
          for val = (progn (pop id-plist) (pop id-plist))
          do (setf (svref hk i) val)
          while id-plist)
    hk))

;;;
;;; OBJECT CREATION
;;; 
;;;; (port:define-dspec-alias define-object-expander (name)
;;;;   `(defun ,name))

(add-object-expander 'cclass-expander most-negative-fixnum)
(defun cclass-expander (form env)
  (labels ((standard-class-p (o)
             (ifit (b-compute-form-type o env)
                 (subtypep it 'standard-class)))
           (ctor-object-form-p (o)
             (and (object-form-p o) 
                  (standard-class-p (object-form-object o))))
           (precompute-class (o)
             (if (globalp o env) (global-value o env)))
           (create-instance-form (class-form args initializers)
             (let* ((idplist    '#:idplist)
                    (key        '#:key)
                    (classvar   '#:class)
                    (class      (precompute-class class-form))
                    (idform     (if class `(,(cclass-ctor-args-fn class) ,@args)
                                  `(funcall (cclass-ctor-args-fn ,classvar) ,@args)))
                    (init-forms (process-object-initializers initializers)))
               `(kb-transaction
                    (kb-find-or-create  
                     ((,classvar ,(or class class-form))
                      (,idplist ,idform)
                      (,key (cclass-make-hashkey ,classvar ,idplist)))
                     ,key
                     (cclass-create-instance ,classvar ,idplist)
                     ,init-forms
                     ((initialize-concept ,classvar object)))))))
                           
    (let ((1st (first form)))
      (cond
       ;; [[CCLASS ID-ARGS] INITIALIZERS]
       ((ctor-object-form-p 1st)
        (create-instance-form (object-form-object 1st)
                              (object-form-args 1st)
                              (rest form)))
       ;; [CCLASS ID-ARGS]
       ((standard-class-p 1st)
        (create-instance-form 1st
                              (rest form)
                              nil))

       ;; [some-object INITIALIZERS]
       (t `(with-fields ,1st ,@(process-object-initializers (rest form))
             object))))))

(defun cclass-create-instance (class args)
  (let ((object (apply #'make-instance class args)))
    object))

(defun initialize-concept (cclass object)
  ;; add properties to fields hash-table 
  (loop for pclass in (cclass-properties cclass)
        for fld-sym = (pclass-field-symbol pclass)
        if (and (pclass-has-default-value pclass)
                (not (property-bound-p object fld-sym)))
        do   (funcall (pclass-default-value-ctor pclass) object))

  ;; call the implication function
  (concept-implication object)
    
  object)

(defgeneric concept-implication (object))
(defmethod concept-implication (object) (declare (ignore object)) nil)


;;;
;;; CREATE-INSTANCE
;;;

;;;; (defun create-instance (cclass initializer args force-new)
;;;;   "Finds or creates an instance of class on behalf of object constructor syntax"
;;;;   (cond
;;;;    (force-new  (cclass-create-instance-and-initialize cclass args initializer))
;;;;    (t          (let ((hashkey (cclass-make-hashkey cclass args)))
;;;;                  (ifit (kb-find-object hashkey)
;;;;                      (init-instance it initializer)
;;;;                    (cclass-create-instance-and-initialize cclass args initializer hashkey))))))


(defmethod print-object ((c concept-class) stream)
  (let ((*print-case* :downcase))
    (pprint-logical-block (stream nil :prefix (if (not *print-context*) "{" "")
                                  :suffix (if (not *print-context*) "}" ""))
      (print-symbol-name (class-name c) stream)
      
      (unless *print-context*
        (princ " :# " stream)
        (print-unreadable-object (c stream :type t :identity t)
          (prin1 (class-name c) stream))))))

(defmethod slot-symbol ((s symbol))
  (if (slot-symbol-p s) s
    (slot-symbol (symbol-name s))))
    
(defmethod slot-symbol ((s string))
  (multiple-value-bind (new-sym status)
      (intern s +slot-symbol-package+)
    (if (#!eq status :external)
        (export new-sym +slot-symbol-package+))
    new-sym))

(defun slot-symbol-p (s)
  (eq (symbol-package s) +slot-symbol-package+))


