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

;;; File: defcon
;;; Description: defcon, the definer of concepts 
;;;

;;; $Id: defcon.lisp,v 1.3 2008/01/22 16:42:40 amallavarapu Exp $
;;;
(in-package b)





;;;; +---------------------------------------------------------------------+
;;;; +                                                                     +
;;;; +    This code is a huge embarassment. Please avert your eyes.        +
;;;; +                                                                     +
;;;; +---------------------------------------------------------------------+






(eval-when (:compile-toplevel :load-toplevel :execute)

;(defconstant +reserved-field-symbols+ '(:kw :object :value))

(portable:define-dspec-class defcon nil nil)

(defconstant +defcon-lambda-list-keywords+ '(&key &optional &rest &property &method))

(defun cclass-ctor-symbol (name)
  (if (cclassp name) (cclass-ctor-symbol (class-name name))
    (sym name "_CONSTRUCTOR")))

(defmacro defcon (name (&rest options) &rest definition) ; (&rest fields) &body bodies)
  (handler-case 
      (let+ ((doc-str (if (stringp (first definition)) (first definition)))
             (fields  (if doc-str (second definition) (first definition)))
             (bodies  (if doc-str (rest (rest definition)) (rest definition) ))
             ((id-fieldinfos 
               ext-field-args
               ctor-lambda-list
               ctor-plist-code 
               pattern-args-fn)       (process-cclass-fields fields))
             (slots            (mapcar (lambda (ifi) (clos-slot-from-id-fieldinfo name ifi)) id-fieldinfos))
             (body-assoc       (parse-key-list '(nil => where) bodies))
             (ctor-body        (cdr (assoc nil body-assoc)))
             (where            (cdr (assoc 'where body-assoc)))
             (impl-body        (cdr (assoc '=> body-assoc)))
             (ordered-id-flds  (mapcar #'fieldinfo-symbol id-fieldinfos))
             (ctor-symbol      (cclass-ctor-symbol name))
             ((abstract 
               traceable
               exposure
               super-types
               superclasses)   (defcon-process-options options))
             (where-pattern    (defcon-compute-where-pattern superclasses where))
             (pred-fn          (sym name '-p))
             (pred-obj         (gensym "O"))
             (ext-fld-forms    (defcon-compute-extended-field-forms name ext-field-args))
             (all-prop-symbols (defcon-compute-property-symbols superclasses ext-fld-forms))
;             (impl-fn          (if impl-body (implication-fn-name name)))
             (id-field-methods (defcon-compute-id-field-methods name id-fieldinfos))
             (class-property   (cclass-property-symbol name))
             (parent-base-props (mapcar #'class-name 
                                        (remove-if #'null (mapcar #'cclass-base-property superclasses)))))

        `(eval-when (:compile-toplevel :load-toplevel :execute)
             
           (portable:dspec (defcon ,name)
             ;; define the class
             (defclass ,name (,@super-types)
               ,slots
               (:metaclass concept-class)
               ,@(if doc-str `((:documentation ,doc-str))))
              
             ;; define the global value
             (define-global-const ,name (the concept-class (find-class ',name)))
   
             ;; define the class property - used as a base class for matching
             (defclass ,class-property (,@parent-base-props)
               ()
               (:metaclass property-class))

             ;; define the class property global value
             (define-global-const ,class-property (the property-class (find-class ',class-property)))

             ;; OBJECT-CTOR
             ,(defcon-build-concept-ctor-code ctor-symbol superclasses ctor-lambda-list
                                              ctor-plist-code all-prop-symbols ctor-body)
                                       

             ;; initialize the class
             (initialize-cclass-object ,name ',ctor-symbol #',pattern-args-fn 
                                       ',id-fieldinfos ',ordered-id-flds ,abstract ,exposure
                                       ',where-pattern ',pred-fn ,class-property
                                       ,traceable)

             ;; define the predicate:
             (defun ,pred-fn (,pred-obj)
               (typep ,pred-obj ,name))

             ;; and define id field readers:
             ,@id-field-methods

             ;; and define any extended fields (i.e., methods & properties):
             ,@(mapcar #'cdr ext-fld-forms)

             ;; define where-rule
             ,(when (and where-pattern (not abstract))
                (defcon-ctor-rule name ordered-id-flds where-pattern))

             ;; define the implication function
             ,(when (and impl-body (not abstract))
                (defcon-implication-method-code name impl-body))
                ;(defcon-define-implication impl-fn ordered-id-flds
                 ;                          all-prop-symbols impl-body))

             (portable:record-definition '(defcon ,name) (portable:location) t)
             ,name)))
    (error (e) (error "Invalid concept ~S: ~A" name e))))

#+Lispworks
(editor:setup-indent 'defcon 2 2 4)

(defun cclass-property-symbol (cclass-name)
  (sym cclass-name #\. "PROPERTY"))

(defun defcon-compute-id-field-methods (name id-fieldinfos)
  (mapcar (lambda (fi)
            (let* ((field            (fieldinfo-symbol fi))
                   (reader           (field-accessor-fn-name name field))
                   (doc              (ifit (fieldinfo-documentation fi) (list it)))
                   (args             '#:args))
              `(defmethod fld ((object ,name) (?field (eql ,field)) &rest ,args)
                 (declare (ignore ,args))
                 ,@doc
                 (,reader object))))
          id-fieldinfos))
   
(defun defcon-process-options (options)
  (let* ((flags            (remove-if-not #'keywordp options))
         (abstract         (if (member :abstract flags) t))
         (traceable        (unless (member :notrace flags) t))
         (exposure         (cond
                            ((member :internal flags) nil)
                            ((member :export flags) :export)
                            (t                      :expose)))
;         (matchable       (if (not (member :non-matchable flags)) t))
         (super-types      (or (remove-if #'keywordp options) '(concept)))
         (superclasses     (mapcar #'find-class super-types)))
    (whenit (find-if (lambda (o) (not (member o '(:abstract :notrace :internal :expose :export :non-matchable)))) flags)
      (error "Invalid flag: ~S" it))
    (values abstract traceable exposure ;  matchable  -- now ignored
            super-types superclasses)))
            
(defun defcon-compute-where-pattern (supers where)
  (append (apply #'append (mapcar #'cclass-where-patterns supers))
          where))
          
(defun defcon-build-concept-ctor-code (name superclasses lambda-list default-args prop-flds body)
  (flet ((get-local-field-symbol (o lsyms)
           (or (find-lsyms-from-symbol o lsyms)
               (error "Invalid local symbol ~S" o))))
    (let* ((id-values                (gensym "ID-FIELDS"))
           (prop-values              (gensym "PROP-FIELDS"))
           (superclass               (first superclasses))
           (id-slot-smacrolets       (map-plist (lambda (k v) (declare (ignorable v))
                                                  `(,(make-local-field-symbol k)
                                                    (getf ,id-values ,k)))
                                                default-args))
           (prop-slot-smacrolets     (mapcar (lambda (p-fld) `(,(make-local-field-symbol p-fld)
                                                               (getf ,prop-values ,(key p-fld))))
                                             prop-flds))
           (smlets                   (nconc id-slot-smacrolets prop-slot-smacrolets))
           (smlet-bindings           (mapcar #'first smlets))
           (args                     (gensym "ARGS"))
           (super                    (intern "SUPER"))) ; symbol "SUPER" in current package
      `(defun ,name ,lambda-list
         (let ((,id-values   (list ,@default-args))
               (,prop-values ()))
           (flet ((,super (&rest ,args) (declare (ignorable ,args))
                    (apply (cclass-ctor-args-fn ,superclass) ,args)))
             (macrolet ((fields (&rest ,args) (fields-form-error ,args)))
               (symbol-macrolet ,smlets
                 (declare (ignorable ,@smlet-bindings))
                 ,@body)
               (values ,id-values ,prop-values))))))))

(defun defcon-compute-property-symbols (superclasses ext-fld-forms)
  (remove-duplicates 
   (append (mapcar #'car ext-fld-forms)
           (mapcar #'pclass-field-symbol
                   (cclass-properties (first superclasses))))))

(defun fields-form-error (args)
  (error "(fields~{ ~S~}) should be replaced with~{ ~A~}."
         args
         (map-plist (lambda (k v) 
                      (format nil "{~A := ~S} "
                              (make-local-field-symbol k)
                              v))
                    args)))


(defun read-id-field-options (symbol opts)
  (let ((default   ?) 
        (matchable t)
        (doc nil))
    (mapp (lambda (k v)
            (ecase k
              (:=    (assert (missingp default)) (setf default v))
              (:#=   (assert (missingp default)) (setf default `(has-name (fld *name* ,(key symbol)) ,v)))
              (:matchable (assert (member v '(nil t)))
                     (setf matchable v))
              (:documentation (setf doc v))))
          opts)
    (values default matchable doc)))
            

(defun make-id-fieldinfo (flddef lambda-switch)
  (let+ ((symbol                  (first flddef))
         (options                 (rest-safely flddef))
         (type                    (get-type-option (first options)))
         (rest-opts               (if type (rest options) options)) ;; options other than type
         ((default matchable doc) (read-id-field-options symbol rest-opts)))
    (if (not (symbolp symbol))
        (error "Expecting a symbol, but received ~S" symbol))
    (make-fieldinfo nil                   ;; the DEF-CLASS, will be set later               
                    (key symbol)          ;; :SYM
                    nil                   ;; ARGS
                    type                  ;; type               
                    matchable             ;; matchable
                    default               ;; default
                    lambda-switch         ;; e.g., NIL, &key, &optional, &rest  
                    doc)))                ;; doc string

(defun initialize-cclass-object (cclass object-ctor pattern-args-fn 
                                        fieldinfos id-order abstract exposure 
                                        where-patterns pred-fn-name base-property
                                        traceablep)
  ; (cclass-reset-definition cclass) - no longer reset the cclass definition
  (cclass-init-slots cclass object-ctor pattern-args-fn id-order
                     abstract exposure where-patterns base-property)
  (cclass-init-from-superclasses cclass)
  (cclass-add-id-fieldinfos cclass fieldinfos)
  (cclass-check-id-fieldinfos cclass)
  (kb-class-init cclass)            
  (process-name-exposure (class-name cclass) exposure)
  (process-name-exposure pred-fn-name exposure)
  (setf (kb-class-traceable-p cclass) traceablep))

(defun cclass-add-id-fieldinfos (cclass fieldinfos)
  (mapc (lambda (fi)
          (assert (eq (fieldinfo-kind fi) :id))
          (setf (fieldinfo-parent fi) cclass) ; FI created at macroexpand time, but 
          (add-fieldinfo cclass fi))          ; cclass is known at RUN-TIME, so we need to set
        fieldinfos))                          ; fieldinfo-parent now.

(defun implication-fn-name (cclass-name)
  (sym cclass-name ".#=>#"))

;;;; (defun defcon-implication-fn-code (fn-name id-symbols ext-fld-symbols body)
;;;;   (let ((smlets       (mapcar (lambda (fld) `(,(make-local-field-symbol fld)
;;;;                                               (fld object ',fld))) 
;;;;                               (append id-symbols ext-fld-symbols))))
;;;;     ;; define the implication function
;;;;     `(defun ,fn-name (object)
;;;;        (symbol-macrolet ,smlets
;;;;          ,@body))))

(defun defcon-implication-method-code (cclass-type body)
  `(defmethod concept-implication ((object ,cclass-type))
     (when (next-method-p) (call-next-method));; call superclass implications first
     (with-fields object
       (with-add-rule ,cclass-type
         ,@body))))

(defun defcon-ctor-rule (class-name slot-symbols where-patterns)
  (let ((object-args (mapcan (lambda (slot-sym) 
                               `(',slot-sym ,(pattern-var slot-sym)))
                             slot-symbols))
        (ctor-rule      (hidden-symbol class-name 'ctor-rule)))
    `(defrule ,ctor-rule 
       (:and ,@where-patterns) 
       => 
       (OBJECT ,class-name :kw
               ,@object-args))))


(defmacro check-id-field-type (loc-sym type)
  `(let ((val ,loc-sym))
     (unless (xtypep val ',type)
       (b-error "Invalid type for ~A (~S).  Expecting type ~S, but received ~S." 
                ',loc-sym ,loc-sym ',type (type-of val)))))
;;;
;;; My apologies for this horrid run-on function - this is one of
;;;    the very first things I wrote in LISP.  
;;;    I would things differently today.
;;;
(defun process-cclass-fields (fields)
  "Returns multiple values: id-fieldinfo, propinfo, ctor-lambda-list, ctor-plist-code, pattern-lambda, where-patterns
     * Id-Fieldinfo = list of id-fieldinfo structures
     * ext-field-args = list of lists of arguments to generate defprop or defmethod forms
     * Ctor-Lambda-list = lambda-list for the ctor (converts concept's arguments into a plist of the form (':slot-symbol1 argval1 ':slot-symbol2 argval2...)
     * Ctor-plist-code = plist of id-fields + forms which initialize their values
     * Pattern-lambda = lambda-expression for the pattern-ctor concept's arguments into the form ((':F1 . VAL-F1)(...)), removing any sublists which have a VAL of NIL or ? (since these aren't used in pattern matching)."
  (let (ctor-lambda-args
        pattern-lambda-args
        plist
        ext-field-args
        pattern-bindings
        id-fieldinfos
        aux-lambda-args
        current-lambda-switch)    
    (declare (special current-lambda-switch))
    (labels ((ctor-lambda-list-argument (symbol default) ;; (lambda (...lambda-arguments...) 
               (if (and default 
                        (member current-lambda-switch '(&key &optional)))
                   `(,symbol ,default)
                 symbol))

             (pattern-lambda-list-argument (symbol)
               (if (member current-lambda-switch '(nil &optional &key))
                   `(,symbol ?)
                 symbol))

             (pattern-binding-expression (symbol)
               `(cons ',(key symbol) ,symbol))

             (record-field (f)                
               (case current-lambda-switch
                 ((&property &method)  (push (cons current-lambda-switch f) ext-field-args))

                 (t ;; record an id field
                  (let* ((symbol   (car f))
                         (info     (make-id-fieldinfo f current-lambda-switch))
                         ;(type     (fieldinfo-type info))
                         (default  (fieldinfo-default info)))

                    ; no longer (check-field-symbol (id-fieldinfo-symbol info))

                    (push info id-fieldinfos)

                    ;; create argument in ctor-lambda-list
                    (push (ctor-lambda-list-argument symbol default)
                          ctor-lambda-args)
                        
                    ;; push 'SLOT-SYMBOL VALUE onto plist
                   (push `,(key symbol) plist)               
                   (push symbol ;(plist-value-expression symbol type)
                         plist)
                     
                    ;; create argument in pattern-lambda-list
                    (push (pattern-lambda-list-argument symbol)
                          pattern-lambda-args)


                    ;; push (_:slot-sym . _:slot-sym) onto assoc-list
                    (push (pattern-binding-expression symbol) pattern-bindings)))))
               
             (record-lambda-switch (s) ;; &key &optional &rest
               (if (not (new-lambda-switch-ok-p s))
                   (error "Unexpected lambda switch ~A after switch ~A"
                          s current-lambda-switch)
                 (setf current-lambda-switch s))

               (case s
                 ((&property &method)  nil)

                 (otherwise  (push s ctor-lambda-args)
                             (unless (eq s '&rest) 
                                 (push s pattern-lambda-args))))))
                  

      (dolist (f fields)
        (cond ((member f +defcon-lambda-list-keywords+)
               (record-lambda-switch f))

              (t (record-field (if (consp f) f `(,f))))))

      (setf ctor-lambda-args    (append (reverse ctor-lambda-args) 
                                        (reverse aux-lambda-args))
            plist               (reverse plist)
            pattern-lambda-args (process-pattern-lambda-args (reverse pattern-lambda-args))
            pattern-bindings    (reverse pattern-bindings)
            id-fieldinfos       (reverse id-fieldinfos)
            ext-field-args      (reverse ext-field-args)))

    (values 
     id-fieldinfos
     ext-field-args
     ctor-lambda-args
     plist
     `(lambda (,@pattern-lambda-args)
        (remove-if #'missingp 
                   (list ,@pattern-bindings)
                   :key #'cdr)))))
 
(defun process-pattern-lambda-args (args)
  (if (consp (first args))
      (cons '&optional (delete '&optional args))
    args))

(defun check-plist (plist indicators)
  (map-plist (lambda (k v)
               (declare (ignorable v))
               (if (not (member k indicators))
                   (error "~S is not one of ~S" k indicators)))
             plist))

(defun get-type-option (o)
  "Returns o if o is a valid type specifier for a slot"
  (if (or (consp o)
          (and (not (keywordp o))
               (symbolp o)))
      o))

(defun rest-safely (o)
  "Apply rest to o if o is a list" 
  (if (consp o) (rest o)))
    
(defun new-lambda-switch-ok-p (k) 
  "determines whether the new lambda switch k is valid given the current switch"
  (declare (special current-lambda-switch))
  (position current-lambda-switch
            (case k
              (&key '(nil &optional &rest))
              (&optional '(nil))
              (&property '(nil &key &optional &rest &method))
              (&method '(nil &key &optional &rest &property))
              (&rest '(nil &optional)))))

(defun clos-slot-from-id-fieldinfo (name id-fieldinfo)
  (let* ((field    (fieldinfo-symbol id-fieldinfo))
         (slot-sym (slot-symbol field))
         (reader   (field-accessor-fn-name name field))
         (type     (fieldinfo-type id-fieldinfo)))
    `(,slot-sym :initarg ,field :reader ,reader :type ,type)))

(defun defcon-compute-extended-field-forms (cclass eargs)
  "Returns a list of cons pairs of the form (FIELD-SYMBOL . FIELD-DEFINITION-FORM),
where FIELD-DEFINITION-FORM is (defprop ...) or (defmethod ...)"
  (loop for e in eargs
        for switch = (car e)
        for form = (cdr e)
        when (eq switch '&property)
        collect (compute-defprop-form cclass form)
        when (eq switch '&method)
        collect (compute-defield-form cclass form)))

(defun compute-defprop-form (cclass dp-args)
  (let* ((prop-fld  (key (car dp-args)))
         (prop-name `(fld ,cclass ,prop-fld)))
    `(,prop-fld defprop ,prop-name ,(rest dp-args))))

(defun compute-defield-form (cclass dm-args)
  (let* ((fn-fld  (key (car dm-args)))
         (fn-name `(fld ,cclass ,fn-fld)))        
    `(,fn-fld defield ,fn-name ,@(rest dm-args))))
                   

(defun cclass-init-slots (cclass ctor-args pattern-args id-order
                                 abstract exposure-val where base-prop)
  (with-slots (ctor-args-fn pattern-args-fn id-field-order
               abstract-p exposure where-patterns base-property) cclass
    (setf ctor-args-fn           ctor-args
          pattern-args-fn        pattern-args
          id-field-order         id-order
          abstract-p             abstract
          exposure               exposure-val
          where-patterns         where
          base-property          base-prop)))


(defun xtypep (obj type)
  (cond
   ((allow-type-p type)  (or (null obj) (xtypep obj (allow-type-type type))))
   
   (t (typep obj type))))              
)


;;;; (defun defcon-build-concept-ctor-code (name super-types lambda-list default-args body prop-flds)
;;;;   (flet ((get-local-field-symbol (o lsyms)
;;;;            (or (find-lsyms-from-symbol o lsyms)
;;;;                (error "Invalid local symbol ~S" o))))
;;;;   (let* ((id-values                (gensym "ID-FIELDS"))
;;;;          (prop-values              (gensym "PROP-FIELDS"))
;;;;          (id-slot-smacrolets       (map-plist (lambda (k v) 
;;;;                                                 `(,(make-local-field-symbol k)
;;;;                                                   (getf ,id-values ,k)))
;;;;                                               default-args))
;;;;          (prop-slot-smacrolets     (mapcar (lambda (p-fld) `(,(make-local-field-symbol p-fld)
;;;;                                                          (getf ,prop-values ,(key p-fld))))
;;;;                                            prop-flds))
;;;;          (superclass               (first super-types))
;;;;          (args                     (gensym "ARGS"))
;;;;          (ctor-args                (gensym "CTOR-ARGS"))
;;;;          (super                    (intern "SUPER"))) ; symbol "SUPER" in current package
;;;;   `(defmethod cclass-construct ((cclass (eql ,name)) &rest ,ctor-args)
;;;;      (destructuring-bind ,lambda-list ,ctor-args
;;;;        (let ((,id-values   (list ,@default-args))
;;;;              (,prop-values ()))
;;;;          (flet ((,super (&rest ,args) (apply (cclass-ctor-args-fn ,superclass) ,args)))
;;;;            (macrolet ((fields (&rest ,args) (fields-form-error ,args)))
;;;;              (symbol-macrolet ,(nconc id-slot-smacrolets prop-slot-smacrolets)
;;;;                ,@body)
;;;;              (values ,id-values ,prop-values)))))))))

;;;; (defun check-field-symbol (f)
;;;;   (when (or (member f +reserved-field-symbols+)
;;;;             (char= #\_ (elt (symbol-name f) 0)))
;;;;     (error "Attempt to use reserved symbol (~A) for field" f)))
