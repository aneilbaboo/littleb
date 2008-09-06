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


;;; File: field
;;; Description: 

;;; $Id: field.lisp,v 1.22 2008/09/06 00:23:08 amallavarapu Exp $
;;;
(in-package b)

(define-method-combination field-combination ()
  ((primary     () :required nil)
   (before      (:before))
   (after       (:after))
   (around      (:around))
   (error       (:error))
   (default     (:default)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((primary (append primary default error))
           (form    (if (or before after (rest primary))
                        `(multiple-value-prog1
                             (progn ,@(call-methods before)
                               (call-method ,(first primary)
                                            ,(rest primary)))
                           ,@(call-methods (reverse after)))
                      `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
        form))))

(defgeneric FLD (object field &rest args)
  (:method-combination field-combination)
  (:documentation "Methods of this generic function implement behavior for object.(field args) syntax"))

(defgeneric (SETF FLD) (value object field &rest args)
  (:method-combination field-combination)
  (:argument-precedence-order object field value)
  (:documentation "Methods of this generic function implement behavior for (setf object.(field args) value) syntax"))

(defmethod fld (o f &rest args) 
  (declare (ignorable o f args))
  (call-next-method))
(defmethod (setf fld) (value o f &rest args)   
  (declare (ignorable value o f args)) 
  (call-next-method))

(defmethod fld ((o null) f &rest args)
  (declare (ignorable o args))
  (cond 
   ((next-method-p) (call-next-method))
   (t               (b-error "Object missing - attempted to access ~S"
                             (list* 'fld nil f args)))))
(port:define-dspec-class defield () "")
(port:define-dspec-form-parser defield (fe)
  (defield-dspec fe))

(defun defield-dspec (ff)
  (if (fld-setf-form-p ff) `(defield (setf ,(fld-form-to-symbol (second ff))))
    `(defield ,(fld-form-to-symbol ff))))
  
(defmacro defield (fe &body method-body)
  (let ((body `(port:dspec ,(defield-dspec fe)
                 (define-field-method ,fe ,@method-body))))
    #+:clisp `(ext:without-package-lock () ,body)
    #-:clisp body))

(defmacro define-field-method (fe &rest method-body)
  `(define-field ,fe (t &method nil) ,@method-body))

(defun setf-fld-form-p (o)
  (and (consp o) (eq (first o) 'setf)
       (fld-form-p (second o))))



(defmacro define-field (name (type lambda-switch matchable &optional update-fn)
                           &body method-body)
  (unless (or (fld-form-p name)
            (setf-fld-form-p name)) 
    (b-error "Invalid name to define-field: ~S.  Expecting a field form." name))
  (let+ (((ff setfp) (if (fld-setf-form-p name)
                         (values (second name) t)
                       (values name nil)))
          (class     (fld-form-object ff))
          (field     (fld-form-field ff)))
    (destructure-method-body (qualifiers args doc-decl code
                                         specializers non-specializers)
        method-body
      (check-field-def-args specializers)
      (let* ((dspec-name (fld-form-to-symbol ff))
             (types      (mapcar (lambda (o) (if (consp o) (second o) t)) specializers))
             (dspec      (if setfp `(method (setf ,dspec-name) ,@qualifiers ,types)
                           `(method ,dspec-name ,@qualifiers ,types))))
      `(portable:dspec ,dspec
         (portable:record-definition ',dspec (portable:location))
         ,@(compute-field-def-forms1 class type field setfp args
                                     qualifiers doc-decl code)
         (add-fieldinfo-data (find-class ',class) 
                             ,field ',args ',type ,matchable nil 
                             ',lambda-switch ,update-fn)
         ',ff)))))

(defun field-accessor-fn-name (class field &optional (export t))
  (let ((name (sym class #\. field)))
    (when export (export name))
    name))
      
(defun compute-field-def-forms1 (class type field setfp args 
                                       qualifiers doc-decl code )
  (declare (ignorable type))
  (let* ((all-args           (gensym "ALL-ARGS"))
         (fn-symbol          (field-accessor-fn-name class field))
         (default-field-p    (when (eq field :?field)
                               (assert (null qualifiers) ()
                                 "Qualifiers ~S are invalid on default field ~S.?field" class)
                               t))
         (qualifiers         (if default-field-p '(:default) qualifiers)))
                               
    (cond 
     (setfp 
      (let* ((value-arg      (first args))
             (rest-args      (rest args))
             (fn-name        `(setf ,fn-symbol))
             (fn-args         (if default-field-p `(,value-arg object ?field ,@rest-args)
                                `(,value-arg object ,@rest-args)))
             (method-name    '(setf fld))
             (method-args    (if default-field-p `(,value-arg (object ,class) ?field &rest ,all-args)
                                 `(,value-arg (object ,class) (?field (eql ,field)) &rest ,all-args)))
             (apply-args      (if default-field-p
                                  `(,value-arg object ?field ,all-args)
                                `(,value-arg object  ,all-args))))
        (compute-field-def-forms2 fn-name field fn-args code method-name method-args
                                  qualifiers doc-decl apply-args)))
         
     (t ; not setf
      (let* ((fn-name     fn-symbol)
             (fn-args     (if default-field-p `(object ?field ,@args)
                            `(object ,@args)))
             (method-name 'fld)
             (method-args (if default-field-p `((object ,class) ?field &rest ,all-args)
                            `((object ,class) (?field (eql ,field)) &rest ,all-args)))
             (apply-args  (if default-field-p `(object ?field ,all-args)
                            `(object ,all-args))))
                            
        (compute-field-def-forms2 fn-name field
                                  fn-args code method-name method-args 
                                     qualifiers doc-decl apply-args))))))

(defun default-field-p (field)
  (eq field :?field))

(defun check-field-def-args (args)
  (dolist (a args)
    (assert (not (consp a)) ()
      "Invalid argument ~S - specializer arguments not allowed." a)))

#+:|LISPWORKS5.0| (defvar *call-next-method* nil)
(defun compute-field-def-forms2 (fn-name field fn-args code method-name method-args
                                         qualifiers doc-decl apply-args)
  (let ((default-p (eq field :field )))
    `(;; define a TYPE.FLD fn which takes object as first arg
      (expose-symbol ',fn-name)
      (defun ,fn-name ,fn-args
        ,@doc-decl
        (declare (special *call-next-method* ,@(if default-p '?field)))
        (flet ((super (&rest args)
                 (apply *call-next-method* object ,(if default-p '?field `',field) args)))
          (with-fields object
            ,@code)))
    
      ;; define the FLD method:
      (defmethod ,method-name ,@qualifiers ,method-args
        ,@doc-decl
        (apply #',fn-name ,@apply-args)))))

;;;;
;;;; STANDARD FLD METHODS:
;;;;
;;;;

;;;
;;; the special FLD :apply method:
;;;
(defun fix-apply-args (args)
  (let ((last2   (last args 2)))
    (setf (cdr last2) (second last2))
    args))

(defmethod fld :around (object (field (eql :apply)) &rest args)
  (declare (ignorable field))
  (apply #'apply #'fld object args))

(defmethod (setf fld) :around (value object (field (eql :apply)) &rest args)
  (declare (ignorable field))
  (call-next-method value object (fix-apply-args args)))

;;;
;;; ANONYMOUS FLD FUNCTIONS are generated by ?.(fld ...) syntax
;;;     ?.X and ?.(X args) returns a function which accesses field X
;;;
;;;     (funcall ?.X O)     == O.X
;;;     (funcall ?.(X 1) O) == O.(X 1)
;;;
;;;     Note: (eq ?.X ?.X)        returns T
;;;           (eq ?.(X 1) ?.(X 1) returns T
;;;           (eq ?.(X)   ?.X     returns T
;;;           (eq ?.X     ?.(X 1) returns NIL
;;;           (eq ?.X     ?.Y     returns NIL
;;;           (eq ?.X     (let ((f :X))
;;;                         ?.,f))  returns T
#+:allegro (eval-when (:compile-toplevel :load-toplevel :execute)
             (mop:finalize-inheritance (find-class 'standard-object)))
(defclass anonymous-fld-function ()
  ((object :initarg :object :reader anonymous-fld-function-object)
   (field :initarg :field :reader anonymous-fld-function-field)
   (args :initarg :args :reader anonymous-fld-function-args))
  (:metaclass port:funcallable-standard-class))

(set-pprint-dispatch 'anonymous-fld-function 'anonymous-fld-function-printer most-positive-fixnum)
(defun make-anonymous-fld-function-key (object field args)
  (vector 'anonymous-fld-function object field args))

(defun make-anonymous-fld-function (object field args fn)
  (let ((hk (make-anonymous-fld-function-key object field args)))
    (or (gethash hk +objects+)
        (setf (gethash hk +objects+) 
              (let ((ffun (make-instance 'anonymous-fld-function :object object :field field :args args)))
                (port:set-funcallable-instance-function 
                 ffun fn)
                ffun)))))

(defun anonymous-fld-function-printer (stream o)
  (let ((*print-case* :downcase))
    (print-object
     (list* 'fld 
            (anonymous-fld-function-object o)
            (anonymous-fld-function-field o)
            (anonymous-fld-function-args o)) stream)))

(defmethod print-object ((o anonymous-fld-function) stream)
  (anonymous-fld-function-printer stream o))

(defmethod fld ((object (eql ?)) field &rest args)
  (make-anonymous-fld-function object field args (lambda (o) (apply #'fld o field args))))

(defmethod fld ((object anonymous-fld-function) field &rest args)
  (make-anonymous-fld-function object field args (lambda (o) (apply #'fld (funcall object o)
                                                          field args))))

;;;
;;; FLD access to FIELDINFOs
;;;
(def-multitype-method fld ((o standard-class built-in-class) (field (eql :_fieldinfos)) &rest args)
  (declare (ignorable field args))
  (class-all-fieldinfos o))
(def-multitype-method fld ((o standard-class built-in-class) (field (eql :_fieldinfo)) &rest args)
  (declare (ignorable field))
  (apply #'class-fieldinfo o args))
(def-multitype-method fld ((o standard-class built-in-class) (field (eql :_fields)) &rest args)
  (declare (ignorable field args))
  (class-fields o))

(defmethod fld (o (field (eql :_fieldinfos)) &rest args)
  (declare (ignorable field args))
  (fieldinfos o))
(defmethod fld (o (field (eql :_fieldinfo)) &rest args)
  (declare (ignorable field))
  (apply #'fieldinfo o args))
(defmethod fld (o (field (eql :_fields)) &rest args)
    (declare (ignorable field args))
    (fields o))

;;;
;;; for accessing fld methods on classes
;;;
(def-multitype-method fld ((o standard-class built-in-class) field &rest args)
    (declare (ignorable field args))
  (find-method #'fld () (list o `(eql ,field))))

;;;
;;; for read/write hash-tables
(defmethod fld ((object hash-table) (field t) &rest args)
  (gethash field object (first args)))

(defmethod (setf fld) (value (object hash-table) (field t) &rest args)
  (declare (ignorable args))
  (setf (gethash field object) value))

(defmethod fld ((object hash-table) (field (eql '*bracket*)) &rest args)
  (gethash (first args) object))

(defmethod (setf fld) (value (object hash-table) (field (eql '*bracket*)) &rest args)
  (declare (ignorable args))
  (setf (gethash field object) value))

;;;
;;; FLD error methods
;;;
(defmethod fld :error (object field &rest args)
  (field-method-not-defined-error object field args nil))

(defmethod (setf fld) :error (value object field &rest args)
  (declare (ignorable value))
  (field-method-not-defined-error object field args t))

;;;
;;; FIELD-METHOD-NOT-DEFINED error:
;;;
(define-condition field-method-not-defined-error (b-error)
  ((id :initarg :id :reader field-method-not-defined-error-id)
   (setfp  :initarg :setfp  :reader field-method-not-defined-setfp))
  (:default-initargs :format-string "Attempt to ~A ~A.  ~A"))

#+:clisp (clos:finalize-inheritance (find-class 'field-method-not-defined-error))
(defmethod print-object ((e field-method-not-defined-error) stream)
  (declare (ignorable stream))
  (with-slots (id setfp format-arguments) e
    (let* ((object (field-method-not-defined-error-object e))
           (field  (field-method-not-defined-error-field e))
           (class  (class-of object)))
      (with-print-context t
        (setf format-arguments
              (list (if setfp "set" "access")
                    (with-output-to-string (s) (print-name id s))
                    (with-print-context nil
                      (cond
                       ((null (class-fieldinfo class field :errorp nil))
                        (format nil "Field ~S is undefined in ~S." field class))
                       
                       (t (format nil "Field ~S may not be ~A in ~S." 
                                  field (if setfp "set" "accessed") class) ))))))
      (call-next-method))))

(defun field-method-not-defined-error-object (e)
  (fld-form-object (field-method-not-defined-error-id e)))

(defun field-method-not-defined-error-field (e)
  (fld-form-field (field-method-not-defined-error-id e)))

(defun field-method-not-defined-error-args (e)
  (fld-form-args (field-method-not-defined-error-id e)))

(defun field-method-not-defined-error-class (e)
  (fld-form-object (field-method-not-defined-error-field e)))

(defun field-method-not-defined-error (obj field args setfp)
  (error 'field-method-not-defined-error 
         :id `(fld ,obj ,field ,@args)
         :setfp setfp))

;;;
;;; FIELD EXPRESSIONS - are lists of the form (FIELD o f a1 a2.. an) or (FIELD o f) 
;;;                                           where o, f and a? are lisp expressions
;;;

;;; Accessors of parts of the fld-formession

;; fld-form-OBJECT :#= O.F1..Fn-1 in O.F1..Fn-1.Fn
(defun fld-form-object (o)
  "Returns the object form."
  (assert (fld-form-p o))
  (second o))

(defun (setf fld-form-object) (value o)
  (assert (fld-form-p o))
  (setf (second o) value))

(defun fld-form-field (o)
  "Returns the field form."
  (assert (fld-form-p o))
  (third o))

(defun (setf fld-form-field) (value o)
  (assert (fld-form-p o))
  (setf (third o) value))

(defun fld-form-args (o)
  "Returns the forms which compute the arguments to a function call"
  (assert (fld-form-p o))
  (cdddr o))

(defun (setf fld-form-args) (value o)
  (assert (fld-form-p o))
  (setf (cdddr o) value))

;; fld-form-HEAD :#= HEAD.FIELD.FIELD.FIELD
(defun fld-form-head (fe)
  (assert (fld-form-p fe))
  (let ((o (fld-form-object fe)))
    (if (fld-form-p o) (fld-form-head o) o)))

(defun (setf fld-form-head) (value fe)
  (let ((o (fld-form-object fe)))
    (if (fld-form-p o) (setf (fld-form-head o) value)
      (setf (fld-form-object fe) value))))

;; fld-form-TAIL :#= HEAD.TAIL where TAIL := FIELD.FIELD...
(defun fld-form-tail (fe)
  "Returns the list of fields in order of access"
  (assert (fld-form-p fe))
  (labels ((calc-tail (f &optional tail)
             (let ((fld (fld-form-field f))
                   (o   (fld-form-object f)))
               (if (fld-form-p o)
                   (calc-tail o (cons fld tail))
                 (list fld)))))
    (reverse (calc-tail fe))))

;;;
;;; predicates:
;;;
(defun fld-form-p (o)
  "Is o a field expression?"
  (and (consp o)
       (eq (first o) 'FLD)
       (consp (cdr o))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let (#+Lispworks (lispworks:*handle-warn-on-redefinition* nil))
    (deftype fld-form ()
      `(satisfies fld-form-p))))

;;;; (defun fld-accessor-p (o)
;;;;   "Returns t if o is a field accessor.  A field accessor is a unary function which retrieves a particular field from its argument.  
;;;; E.g., (funcall ?.A myobj) is equivalent to (funcall (lambda (o) (FLD O :A)) myobj)"
;;;;   (and (consp o)
;;;;        (eq (first o) 'lambda)
;;;;        (eq (second o) *fld-accessor-lambda-list*)))

;; not a recursive field structure
(defun unary-fld-form-p (o)
  "If only one field is accessed."
  (and (fld-form-p o)
       (not (fld-form-p (fld-form-object o)))))

;; GLOBAL-CONST-fld-form :#= GLOBAL.FIELD
(defun global-const-fld-form-p (o)
  "True if the object is a global symbol and the field is a const-arg"
  (and (fld-form-p o)
       (global-const-p (fld-form-object o))
       (compile-time-computable-form-p (fld-form-field o))))

;; CONST-fld-form :#= GLOBAL.FIELD.FIELD.FIELD...
(defun const-fld-form-p (o)
  (and (fld-form-p o)
       (let ((obj (fld-form-object o)))
         (or (global-block-p obj)
             (global-const-p obj)))))

(deftype quoted-symbol ()
  `(cons (eql quote) (cons symbol null)))
(defun print-const-fld-form (fe &optional stream)
  (let ((o    (fld-form-object fe))
        (fld  (fld-form-field fe)))
    (if (fld-form-p o) 
        (print-const-fld-form o stream)
      (prin1 o stream))
    (princ #\. stream)
    (typecase fld
      (keyword       (princ fld stream))
      (quoted-symbol (prin1 fld stream))
      (t             (error "Expecting a const-fld-form")))))

;;; Helper funcs
(defun fld-form-to-symbol (ff)  
  (let* ((o    (fld-form-object ff))
         (a    (fld-form-args ff)))
    (unless (and (null a)
                 (or (symbolp o) (fld-form-p o)))
      (b-error "Cannot convert ~S to a symbol." ff))
    (let ((pkg  (symbol-package (fld-form-head ff))))
      #+clisp
      (ext:without-package-lock ("COMMON-LISP")
       (intern (with-output-to-string (s)
                 (print-const-fld-form ff s)) pkg))
      #-clisp
      (intern (with-output-to-string (s)
                (print-const-fld-form ff s)) pkg))))

(defun fld-setf-form-p (o)
  "Returns T if O is of form (SETF FE), where FE is a field expression"
  (and (consp o)
       (eql (first o) 'setf)
       (fld-form-p (second o))))

;;;
;;; Support for LISPWORK's Find Source capability - make it work with FLD expressions
;;; 
(defmacro define-field-form-parser (definer)
  (let ((name (gensym "NAME")))
    `(portable:define-dspec-form-parser ,definer (,name &rest ,(gensym))
       (declare (ignorable ,definer))
       `(,',definer ,(if (fld-form-p ,name)
                      (fld-form-to-symbol ,name)
                    ,name)))))


(defun get-method-body-dspec-info (subform-fn)
  (flet ((specializer-types (lambda-list) ()
           (loop for arg in lambda-list
                 while (not (member arg lambda-list-keywords))
                 collect (if (consp arg) (second arg) t))))
    (loop for next-form = (funcall subform-fn)
          while (not (listp next-form))
          collect next-form into quals
          finally (let ((types (specializer-types next-form)))
                    (return (values quals types))))))

(portable:define-dspec-form-parser defun (name)
  (declare (ignorable defun))
  `(defun ,(if (fld-form-p name) (fld-form-to-symbol name) name)))

(portable:define-dspec-form-parser defmethod (name &rest subform-fn)
  (declare (ignorable defmethod))
  (let+ (((quals types) (get-method-body-dspec-info subform-fn)))
    (if (fld-form-p name) `(method ,(fld-form-to-symbol name) ,@quals ,types)
      `(method ,name ,@quals ,types))))
                       

(defmethod fld (o (field (eql :coerce)) &rest args)
  (destructuring-bind (result-type) args
    (coerce o result-type)))
;;;
;;; Default bracket fields - for accessing elements of sequences
;;;
(defmethod fld ((o sequence) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a sequence"
  (destructuring-bind (index) indexes
    (elt o index)))

(defmethod (setf fld) (value (o sequence) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a sequence"
  (destructuring-bind (index) indexes
    (setf (elt o index) value)))

(defmethod fld ((o vector) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a vector"
  (destructuring-bind (index) indexes
    (svref o index)))

(defmethod (setf fld) (value (o vector) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a vector"
  (destructuring-bind (index) indexes
    (setf (svref o index) value)))


(defmethod fld ((o list) (field (eql :subsetp)) &rest args)
  (destructuring-bind (list2 &rest subsetp-args &key key (test (quote eql) testp) (test-not nil notp)) args
    (declare (ignore key test test-not))
    (apply #'subsetp o list2 subsetp-args)))

(defmethod fld ((o list) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a list"
  (destructuring-bind (index) indexes
    (nth index o)))

(defmethod (setf fld) (value (o list) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a list"
  (destructuring-bind (index) indexes
    (setf (nth index o) value)))

(defmethod fld ((o string) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a string"
  (destructuring-bind (index) indexes
    (char o index)))

(defmethod (setf fld) (value (o string) (field (eql '*bracket*)) &rest indexes)
  "Accesses the 0-based element of a string"
  (destructuring-bind (index) indexes
    (setf (char o index) value)))

(defmethod fld ((o array) (field (eql '*bracket*)) &rest indexes)
  "Accesses an element of an array"
  (apply #'aref o indexes))

(defmethod (setf fld) (value (o array) (field (eql '*bracket*)) &rest indexes)
  "Accesses an element of an array"
  (setf (apply #'aref o indexes) value))

(defmacro def-lisp-field (type fn-name &key (field-name (key fn-name)) (object 0))
  `(defmethod fld ((object ,type) (field (eql ,field-name)) &rest args)
     (declare (ignore field))
     ,(ecase object
      (0
       `(apply #',fn-name object args))
      (1
       `(apply #',fn-name (first args) object (rest args)))
      (2
       `(apply #',fn-name (first args) (Second args) object (cddr args))))))

(defmacro def-lisp-fields (type &rest flddefs)
  `(progn 
     ,@(mapcar (lambda (flddef)
                 `(def-lisp-field ,type ,@(ensure-list flddef)))
               flddefs)))
                 
(defmethod fld ((o sequence) (field (eql :from-end)) &rest args)
  (destructuring-bind (n) args
    (elt o (- (length o) 1 n))))

      
(def-lisp-fields sequence
                 length
                 fill
                 subseq
                 (map :object 2)
                 map-into
                 (reduce :object 1)
                 (count :object 1)
                 (count-if :object 1)
                 (count-if-not :object 1)
                 reverse nreverse
                 sort stable-sort
                 (find :object 1)
                 (find-if :object 1)
                 (find-if-not :object 1)
                 (position :object 1)
                 (position-if :object 1)
                 (position-if-not :object 1)
                 (search :object 1)
                 (mismatch :object 1)
                 (replace :object 1)
                 (substitute :object 2) 
                 (substitute-if :object 2) 
                 (substitute-if-not :object 2) 
                 (nsubstitute :object 2) 
                 (nsubstitute-if :object 2)  
                 (nsubstitute-if-not :object 2) 
;               (concatenate 
;               merge
                 (remove :object 1)
                 (remove-if :object 1)
                 (remove-if-not :object 1)
                 (delete :object 1)
                 (delete-if :object 1)
                 (delete-if-not :object 1)
                 remove-duplicates 
                 delete-duplicates)

(defmethod fld ((seq sequence) (field (eql :alpha-order)) &rest args)
  (destructuring-bind (&key reverse (printer #'prin1) key case-insensitive) args
    (sort seq
          (lambda (str1 str2)
            (string-alpha< str1 str2 :reverse reverse :case-insensitive case-insensitive))
          :key (if key (lambda (x) (with-output-to-string (string)
                                     (funcall printer (funcall key x) string)))
                  (lambda (x) (with-output-to-string (string)
                                (funcall printer x string)))))))
          

(defun string-alpha< (seq1 seq2 &key reverse case-insensitive)
  (loop with test< =  (if case-insensitive
                          (if reverse #'char-greaterp #'char-lessp)
                        (if reverse #'char> #'char<))
        with test= = (if case-insensitive #'char-equal #'char=)
        for c1 across seq1
        for c2 across seq2
        unless (funcall test= c1 c2)
        if (funcall test< c1 c2) 
        return t
        else return nil
        finally (return (< (length seq1) 
                           (length seq2)))))

(defmethod fld ((list1 sequence) (field (eql :multiset-same-p)) &rest args)
  (destructuring-bind (list2 &key (test 'eql)) args
    (multiset-same-p (coerce list1 'list) (coerce list2 'list) :test test)))

(def-lisp-fields list
                 multiset-same-p 
                 exclusion
                 nexclusion
                 car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
                 caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar 
                 cddadr cdddar cddddr 
                 (mapcar :object 1) (mapc :object 1) (maplist :object 1) (mapl :object 1)
                 (mapcan :object 1) (mapcon :object 1)
                 first second third fourth fifth sixth seventh eighth ninth
                 last butlast nthcdr)

(def-lisp-fields number
                 + - / * 1+ 1-
                 > < = >= <= /=
                 ash zerop plusp minusp
                 floor ffloor ceiling fceiling truncate ftruncate round fround
                 sin cos tan asin acos atan
                 abs evenp oddp exp expt
                 gcd)
;;;; (port:define-dspec-class defield () "")
;;;; (port:define-dspec-form-parser defield (fe &body method-body)
;;;;   (if (fld-setf-form-p fe) `(defield (setf ,(fld-form-to-symbol (second fe))))
;;;;     `(defield ,(fld-form-to-symbol fe))))

;;;; (defmacro destructure-field-dspec ((name method-body) (&optional (dspec '#:dspec)
;;;;                                                                  (ff '#:ff) (setfp '#:setfp)
;;;;                                                                  (class '#:class)
;;;;                                                                  (doc-decl '#:doc-decl)
;;;;                                                                  (field '#:field)
;;;;                                                                  (args '#:args)) &body body)
;;;;   (let ((qualifiers '#:qualifiers)
;;;;         (code '#:code)
;;;;         (specializers '#:specializers)
;;;;         (non-specializers '#:non-specializers)
;;;;         (dspec-name '#:dspec-name)
;;;;         (types '#:types))
;;;;      `(progn
;;;;         (unless (or (fld-form-p ,name)
;;;;                     (setf-fld-form-p ,name)) 
;;;;           (b-error "Invalid name to define-field: ~S.  Expecting a field form." ,name))
;;;;   
;;;;         (let+ (((,ff ,setfp) (if (fld-setf-form-p ,name)
;;;;                                  (values (second ,name) t)
;;;;                                (values ,name nil)))
;;;;                (,class     (fld-form-object ,ff))
;;;;                (,field     (fld-form-field ,ff)))
;;;;           (destructure-method-body (,qualifiers ,args ,doc-decl ,code
;;;;                                                 ,specializers ,non-specializers)
;;;;               ,method-body
;;;;             (check-field-def-args ,specializers)
;;;;             (let* ((,dspec-name  (fld-form-to-symbol ,ff))
;;;;                    (,types       (mapcar (lambda (o) (if (consp o) (second o) t)) ,specializers))
;;;;                    (,dspec       (if setfp `(method (setf ,,dspec-name) ,@,qualifiers ,,types)
;;;;                                    `(method ,,dspec-name ,@,qualifiers ,,types))))
;;;;               ,@body))))))

;;;; (defmacro define-field (name (type lambda-switch matchable &optional update-fn)
;;;;                            &body method-body)
;;;;   (destructure-field-dspec (name method-body) (dspec ff setfp class field args)
;;;;     `(portable:dspec ,dspec
;;;;        (portable:record-definition ',dspec (portable:location))
;;;;        ,@(compute-field-def-forms1 class type field setfp args
;;;;                                    qualifiers doc-decl code)
;;;;        (add-fieldinfo-data (find-class ',class) 
;;;;                            ,field ',args ',type ,matchable nil 
;;;;                            ',lambda-switch ,update-fn)
;;;;        ',fe)))