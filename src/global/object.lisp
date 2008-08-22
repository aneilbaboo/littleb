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

;;; File: object.lisp
;;; Description: defines the OBJECT macro

;;; $Name:  $
;;; $Id: object.lisp,v 1.8 2008/08/22 15:04:52 amallavarapu Exp $
;;;
(in-package b)

(defun initializer-setf-signal-p (o)
  (keywordp o))

(defun initializer-setf-name-signal-p (o)
  (and (initializer-setf-signal-p o)
       (char= (char (symbol-name o) 0) #\#)))

(defun process-object-initializers (i)
  "Initializers are forms which set or access fields in an object.
A list of initializers is of the form:
   [ setf-signal | setf-name-signal | non-keyword-form ]*
   Where setf-signal ::= keyword form  
         setf-name-signal ::= #keyword form
         non-keyword-form ::= any LISP form other than a keyword.
         #keyword ::= keyword with name starting with #."
  (labels ((field-symbol-from-signal (o)
             (make-local-field-symbol
              (subseq (symbol-name o)
                      (if (initializer-setf-name-signal-p o) 1 0)))))
    (loop for iter = i
              then (cdr iter)
          for 1st = (car iter)
          for 2nd = (cadr iter)
          while iter
          if (initializer-setf-name-signal-p 1st)
          collect `(setf-name ,(field-symbol-from-signal 1st)
                          ,2nd)
          and do (setf iter (cdr iter))
          else if (initializer-setf-signal-p 1st)
          collect `(setf ,(field-symbol-from-signal 1st) ,2nd)
          and do (setf iter (cdr iter))
          else collect 1st)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun tree-find-if (fn list &key (key 'identity))
  (find-if (lambda (o) 
             (if (listp o) (tree-find-if fn o :key key)
               (funcall fn (funcall key o))))
           list))
(defun tree-find (x list &key (key 'identity) (test 'eql))
  (tree-find-if (lambda (o) (funcall test (funcall key x) o)) list))
(defmacro with-fields (form &body body &environment env)
  "Provides easy access to fields & field functions defined on the object.
The symbol OBJECT is bound to the object computed by FORM.  Fields may be
accessed with symbols like .field-name."
  (declare (special current-property))
  (cond 
   (body
    (flet ((fld-sym-from-lsym (lsym)
             (key (subseq (symbol-name lsym) 1))))
      (let+ (((new-body lsyms) (convert-local-field-symbols body))
             (obj-var         (gensym "OBJ"))
             (form-type       (b-compute-form-type form env))
             (typed-obj       (if form-type `(the ,form-type ,obj-var) obj-var))
             (smacro-specs    (mapcar (lambda (s) `(,s (fld object 
                                                            ,(fld-sym-from-lsym s))))
                                      lsyms))
             (args            (gensym "ARGS"))
             (value           (gensym "VALUE"))
             (func-specs      (mapcan (lambda (s) 
                                        `((,s (&rest ,args) 
                                              (fld object :apply ,(fld-sym-from-lsym s) ,args))
                                          ((setf ,s) (,value &rest ,args)
                                           (setf (fld object :apply ,(fld-sym-from-lsym s) ,args)
                                                 ,value))))
                                      lsyms))
             (pbound-args     `(,@(if (boundp 'current-property)
                                      `(&optional (1st ,current-property))
                                    `(1st &optional))
                                (2nd nil 2nd-p)))
             (pboundp-def      (if (tree-find 'property-bound-p body)
                                   `((property-bound-p ,pbound-args
                                                       (if 2nd-p (concept-property-bound-p 1st 2nd)
                                                         (concept-property-bound-p object 1st))))))
             (apply-def        (if (tree-find '|.APPLY| body) 
                                   `((|.APPLY| (field &rest args) (fld object :apply field args)))))
             (nth-object-def   (if (tree-find 'nth-object body) `((nth-object (n) (nth n *objects*))))))
        `(let* ((,obj-var ,form)
                (*objects* (cons ,obj-var *objects*)))
           (symbol-macrolet ((object ,typed-obj)
                             ,@smacro-specs)
             (declare (ignorable ,@(mapcar #'first smacro-specs)))
             (flet (,@nth-object-def
                    ,@pboundp-def
                    ,@apply-def
                    ,@func-specs)
               ,@new-body))))))
   (t form)))
)

                       

;;;
;;; PRIORITY LISTs - base functionality for adding/removing prioritized names
;;;                  from a list
;;;
(defun add-prioritized-symbol (name priority priority-list)
  (labels ((add-it (rest-list)
             (let ((head (first rest-list)))
               (cond
                ((or (null head) 
                     (>= priority (cdr head)))
                 (list* (cons name priority) rest-list))
                (t
                 (list* head (add-it (rest rest-list))))))))
    (add-it (remove name priority-list :key #'car))))

(defun remove-prioritized-symbol (name priority-list)
  (remove name priority-list :key #'first))

;;;
;;; OBJECT EXPANDERS - a prioritized list of symbols representing object-expansion functions
;;;
(defvar *object-expanders* '()
  "A cons (FN-NAME . PRIORITY)")       

(defun add-object-expander (expander &optional (priority 0))
 "Adds an expander, to the list of object expanders consulted by the OBJECT expander macro.
  EXPANDER: A function (symbol or fn object) of two arguments: (FORM ENVIRONMENT)
     which returns multiple values:
     ID-FORM: a form which evaluates the arguments of FORM, called the IDENTITY
     KEY-CONSTRUCTOR: a function which generates a key from the IDENTITY
     OBJECT-CONSTRUCTOR: a function which creates an object from the IDENTITY
     ACTIONS:
  Note: ENVIRONMENT is a symbol to which is bound the environment variable which may
     be used inside body in calculating the results
        PRIORITY is a number (or NIL, interpreted as 0) which like set-pprint-dispatch,
     is used to prioritize expanders.  The highest priority & most recently defined
     expander which returns a value is used."
 (setf *object-expanders* (add-prioritized-symbol expander priority *object-expanders*)))

(defun remove-object-expander (expander)
  (setf *object-expanders* (remove-prioritized-symbol expander *object-expanders*)))

(defmacro internal-kb-access-object (mode  bindings
                                           key-form
                                           obj-form
                                           user-forms 
                                           create-forms)
  (let ((found '#:found)
        (key   '#:key))
    (ecase mode
     (:find-or-create  (let* ((add-obj-form `(kb-add-object ,key ,obj-form)))
                         `(let* (,@bindings
                                 (,key      ,key-form)
                                 (,found    (kb-find-object ,key)))
                            (with-fields (or ,found ,add-obj-form)
                              ,@user-forms
                              (unless ,found 
                                ,@create-forms)
                              object))))
     (:fast-query     `(let* (,@bindings)
                         (kb-find-object ,key-form)))
     (:force-create   `(with-find-or-create-object 
                         (let* (,@bindings)
                           (with-fields ,obj-form ,@user-forms ,@create-forms object)))))))

(defmacro kb-find-or-create (bindings key-form obj-form find-actions create-actions)
  "Evaluates ARGS-FORM, storing the result in ARGS.  KEY-FORM should compute a key used for finding the object; OBJ-FORM should be a form which creates a new instance of the object"
  `(internal-kb-access-object :find-or-create ,bindings ,key-form ,obj-form ,find-actions ,create-actions))

(defmacro with-forced-create-object (&body body)
  "installs a version of kb-find-or-create which forces creation of objects 
   (but does not force creation of arguments to the object)"
  `(macrolet ((kb-find-or-create (bindings key-form obj-form find-actions create-actions)
                `(internal-kb-access-object 
                  :force-create ,bindings ,key-form ,obj-form ,find-actions ,create-actions)))
     ,@body))

(defmacro with-find-or-create-object (&body body)
  `(macrolet ((kb-find-or-create (bindings key-form obj-form find-actions create-actions)
                `(internal-kb-access-object
                  :find-or-create ,bindings ,key-form ,obj-form ,find-actions ,create-actions)))
     ,@body))

(defmacro with-fast-query-object (&body body)
  `(macrolet ((kb-find-or-create (bindings key-form obj-form find-actions create-actions)
                `(internal-kb-access-object
                  :fast-query ,bindings ,key-form ,obj-form ,find-actions ,create-actions)))
     ,@body))

(defun object-expansion (args force-new env)
  (or (loop for (expander) in *object-expanders*
            for expansion = (funcall expander args env)
            when expansion
            return (if force-new
                       `(kb-transaction (with-forced-create-object ,expansion))
                     `(kb-transaction ,expansion)))
      (error "Cannot find object expansion for ~S" args)))
;      `(kb-transaction (cclass-expander ,force-new ,(first args) ,@(rest args)))))


(defmacro object (&rest body &environment env)
  (object-expansion body nil env))

(defmacro temporary-object (&rest args &environment env)
  (object-expansion args t env))

;;;
;;; helper functions for with-fields
;;;
(defun make-local-field-symbol (fsym)
  ".F is the local field symbol of the field :F, PKG:.F is the field symbol of PKG:F"
  (typecase fsym
   (keyword (sym #\. fsym))
   (symbol  (intern (mkstr #\. fsym) (symbol-package fsym)))
   (t       (sym #\. fsym))))

(defun local-field-symbol-p (o)
  (and (symbolp o)
       (not (keywordp o))
       (char= (char (symbol-name o) 0) #\.)))

(defun local-field-symbol-form-p (o)
  (or (local-field-symbol-p o)
      (and (consp o)
           (local-field-symbol-p (first o)))))

(defun local-field-symbol-to-keyword (o)
  (assert (local-field-symbol-p o))
  (key (subseq (symbol-name o) 1)))
 
(deftype local-field-symbol ()
  `(satisfies local-field-symbol-p))

(defun convert-local-field-symbols (form)
  (let ((lsyms ()))
    (mapatoms (lambda (a)
                (typecase a
                  (local-field-symbol (push a lsyms) a)
                  (t                  a)))
              form)
    (values form lsyms)))

(defun object-form-p (o)
  "Is o a object expression? (-> object args)"  
  (and (consp o)
       (eq (car o) 'OBJECT)))

(defun object-form-body (o)
  "(object-form-body '[a b c] => (a b c)"
  (cdr o))

(defun (setf object-form-body) (value o)
  (setf (cdr o) value))

(defun object-form-object (o)
  "(object-form-object '[a b c] => a"
  (second o))

(defun (setf object-form-object) (value o)
  (setf (second o) value))

(defun object-form-args (o)
  "(object-form-args '[a b c] => (b c)"
  (cddr o))

(defun (setf object-form-args) (value o)
  (setf (cddr o) value))


(deftype object-form ()
  `(satisfies object-form-p))

