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

;;; File: fieldinfo
;;; Description: fieldinfo structures record arguments & type of a field

;;; $Id: fieldinfo.lisp,v 1.1 2007/09/25 17:54:12 amallavarapu Exp $
;;;
(in-package b)

(defstruct (fieldinfo (:conc-name fieldinfo-)
                      (:constructor _make-fieldinfo))
  parent      ;; the class for which this fieldinfo is defined (may be a superclass of the class)
  symbol        ;; name of this field, a symbol object (usually a keyword)
  args          ;; the field lambda list
  type          ;; the type returned by this field
  matchable     ;; T for fields which can be matched by rules 
                ;; (T for :ID and :PROPERTY, and for :MATCHABLE :METHOD fields)
  default       ;; default value form
  lambda-switch
  documentation)

(defun make-fieldinfo (parent symbol args type matchable default lambda-switch &optional documentation)
  (_make-fieldinfo
   :parent parent
   :symbol symbol
   :args args
   :type (or type t) 
   :matchable matchable
   :default default
   :lambda-switch lambda-switch
   :documentation documentation))

#+:allegro
(defmethod make-load-form ((f fieldinfo) &optional env)
  (with-slots (parent symbol args type matchable default lambda-switch) f
    (let ((form `(fieldinfo-load-form ',parent ',symbol ',args ',type ',matchable ',default ',lambda-switch)))
      form)))

#+:allegro
(defun fieldinfo-load-form (parent symbol args type matchable default lambda-switch)
  (make-fieldinfo parent symbol args type matchable default lambda-switch))

(defun fieldinfo-kind (fi)
  (ecase (fieldinfo-lambda-switch fi)
    ((nil &key &optional &rest)  :id)
    (&property                   :property)
    (&method                     :method)))

(defun id-fieldinfo-p (fi)
  (eq (fieldinfo-kind fi) :id))
(defun property-fieldinfo-p (fi)
  (eq (fieldinfo-kind fi) :property))
(defun method-fieldinfo-p (fi)
  (eq (fieldinfo-kind fi) :method))

(defun fieldinfo (instance field)
  (class-fieldinfo (class-of instance) field))

(defun fieldinfos (instance)
  (class-fieldinfos (class-of instance)))

(defun fields (instance)
  (class-fields (class-of instance)))

(defun class-fields (class)
  (mapcar #'fieldinfo-symbol (class-fieldinfos class )))

(defun class-fieldinfo (class field &optional ensure-initialized)
  (find field (class-all-fieldinfos class ensure-initialized) 
        :key #'fieldinfo-symbol))

(defun (setf class-fieldinfo) (newfi class &optional (field (fieldinfo-symbol newfi)))
  (cond
   (newfi
    (assert (eq field (fieldinfo-symbol newfi)))
    (let* ((existingfis  (class-fieldinfos class))
           (pos          (position field existingfis :key #'fieldinfo-symbol)))
      (if pos
          (setf (nth pos existingfis) newfi)
        (setf (class-fieldinfos class) (nconc existingfis (cons newfi nil))))))
   (t (setf (class-fieldinfos class) 
            (remove field (class-fieldinfos class)
                    :key #'fieldinfo-symbol)))))

(defun class-fieldinfos (class &optional ensure-initialized)
  (assert (typep class 'class))
  (let+ (((fis initialized) (gethash class +fieldinfos+)))
    (cond
     (initialized         fis)
     (ensure-initialized  (setf (gethash class +fieldinfos+) nil))
     (t                   (init-class-fieldinfos class)))))

(defun (setf class-fieldinfos) (newfis class)
  (assert (typep class 'class))
  (setf (gethash class +fieldinfos+) newfis))

(defun class-all-fieldinfos (class &optional ensure-initialized)
  (remove-duplicates (append (class-fieldinfos class ensure-initialized)
                             (let ((supers (ignore-errors (class-all-superclasses class))))
                               (apply #'append (mapcar #'class-all-fieldinfos supers))))
                     :key #'fieldinfo-symbol
                     :from-end t ; retain most derived fieldinfo
                     ))

(defun init-class-fieldinfos (class)
  (loop for super in (portable:class-direct-superclasses class)
        for superfis = (class-fieldinfos super)
        nconc superfis
        do (dolist (fi superfis)
             (add-fieldinfo class fi))))
 
(defun add-fieldinfo (class fi &optional updatefn)
  (check-fieldinfo-for-superclasses class fi)
  (check-fieldinfo-for-subclasses class fi)
  (add-fieldinfo-to-subclasses class fi updatefn))

(defun add-fieldinfo-data (class symbol args type matchable default lambda-switch &optional updatefn)
  (let ((fi      (make-fieldinfo class symbol args type matchable default lambda-switch)))
    (add-fieldinfo class fi updatefn)))

(defun add-fieldinfo-to-subclasses (class fi updatefn)
  (let ((existingfi (class-fieldinfo class (fieldinfo-symbol fi) t)))
    (unless (and existingfi
                 (subtypep #1=(fieldinfo-parent existingfi) 
                           #2=(fieldinfo-parent fi))
                 (not (eq #1# #2#)))
      (setf (class-fieldinfo class) fi)
      (when updatefn
        (funcall updatefn class))
      (dolist (subclass (portable:class-direct-subclasses class))
        (add-fieldinfo-to-subclasses subclass fi updatefn)))))

;;;
;;; code for checking the class and kind of fieldinfos
;;;
(defun check-fieldinfo-for-superclasses (class fi)
  (dolist (super (portable:class-direct-superclasses class))
    (whenit (fieldinfo super (fieldinfo-symbol fi))
      (check-fieldinfo-for-subclass super it class fi))))

(defun check-fieldinfo-for-subclasses (class fi)
  "Also checks that class has the appropriate subtype relationship to any existing fieldinfo"
  (dolist (sub (portable:class-direct-subclasses class))
    (whenit (fieldinfo sub (fieldinfo-symbol fi))
      (check-fieldinfo-for-subclass class fi sub it))))

(defun fieldinfo-error (msg sub-name sub-field sub-args super-name super-field super-args)
  (error msg sub-name sub-field sub-args super-name super-field super-args))

(defun check-fieldinfo-for-subclass (super-class super-fi sub-class sub-fi)
  (let* ((sub-type    (fieldinfo-type sub-fi))
         (sub-kind    (fieldinfo-kind sub-fi))
         (sub-name    (if (symbolp sub-class) 
                        sub-class
                        (class-name sub-class)))
         (sub-args    (fieldinfo-args sub-fi))
         (sub-field   (fieldinfo-symbol sub-fi))
         (super-type  (fieldinfo-type super-fi))
         (super-kind  (fieldinfo-kind super-fi))
         (super-name  (if (symbolp super-class)
                          super-class
                        (class-name super-class)))
         (super-field (fieldinfo-symbol super-fi))
         (super-args  (fieldinfo-args super-fi)))

    (unless (matching-lambda-lists-p sub-args super-args)
      (fieldinfo-error "Lambda-list of ~S.~A ~S does not match that ~S.~A ~S"
                       sub-name sub-field sub-args super-name super-field super-args))

    (unless (subtypep sub-type super-type)
      (fieldinfo-error "Type of ~S.~A (~S) is not a subtype of ~S.~A (~S)"
                       sub-name sub-field sub-type super-name super-field super-type))

    (unless (eq super-kind sub-kind)
      (fieldinfo-error "Kind of ~S.~A (~S) does not match kind of ~S.~A (~S)"
                       sub-name sub-field sub-kind super-name super-field super-kind))))

;;;
;;; code for comparing lambda lists
;;;
(defun lambda-list-var-name (o)
  (if (consp o) (first o) o))

(defun matching-lambda-lists-p (l1 l2)
  (let ((p1 (parse-lambda-list l1))
        (p2 (parse-lambda-list l2)))
    (when (eq (length p1) (length p2))
      (loop for varset1 in p1
            for varset2 in p2
            for keyword1 = (first varset1)
            for keyword2 = (first varset2)
            for vars1 = (mapcar #'lambda-list-var-name (rest varset1))
            for vars2 = (mapcar #'lambda-list-var-name (rest varset2))
            unless (and (eq keyword1 keyword2)
                        (eq (length vars1) (length vars2))
                        (or (not (eq keyword1 '&key))
                            (equalp vars1 vars2)))
            do (return nil)
            finally (return t)))))

(defun parse-lambda-list (ll)
  (loop with vars = nil
        with result = nil
        with keyword = nil
        for a in ll
        if (member a lambda-list-keywords)
        do (push (cons keyword (reverse vars)) result)
           (setf keyword a
                 vars    nil)
        else 
        do (push a vars)
        finally (return (nreverse result))))


;;;
;;; FIELDINFO fields:
;;; 
(defmethod fld ((f fieldinfo) (field (eql :symbol)) &rest args)
  (fieldinfo-symbol f))
(defmethod fld ((f fieldinfo) (field (eql :kind)) &rest args)
  (fieldinfo-kind f))
(defmethod fld ((f fieldinfo) (field (eql :args)) &rest args)
  (fieldinfo-args f))
(defmethod fld ((f fieldinfo) (field (eql :type)) &rest args)
  (fieldinfo-type f))
(defmethod fld ((f fieldinfo) (field (eql :matchable)) &rest args)
  (fieldinfo-matchable f))

;;;
;;; 
;;;

;;;;   (typecase field
;;;;     (keyword (let* ((class-name (class-name (class-of o)))
;;;;                     (slot-name  (intern (symbol-name field)
;;;;                                         (symbol-package class-name))))
;;;;                (slot-value o slot-name)))
;;;;     (t       (slot-value o field))))

;;;;;
;;;;; form type computation - this needs to go elsewhere
;;;;;

(defun b-compute-form-class (form env &optional error)
  "Computes the form class, taking OBJECT expressions into account"
  (let* ((type-form (b-compute-form-type form env error))
         (type      (cond
                     ((allow-expr-p type-form) (allow-type-type type-form))
                     ((symbolp type-form)      type-form))))
    (when type
      (find-class type error env))))

(defun b-compute-form-type (form env &optional error)  
  (cond ((object-form-p form)
         (let ((obj (object-form-object form)))
           (cond
            ((symbolp obj)  (ifit (compute-form-value obj env error) (class-name it) nil))
            (t              (b-compute-form-type obj env)))))

        ((fld-form-p form)
         (let* ((class (b-compute-form-class (fld-form-object form) env error))
                (fld   (compute-form-value (fld-form-field form) env error))
                (fi    (when (and fld class) (class-fieldinfo class fld error))))
           (if fi (fieldinfo-type fi))))

        (t (compute-form-type form env))))
