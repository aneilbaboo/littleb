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

;;; File: rule-patterns
;;; Description: Computes the LISA patterns required to support a little b rule.

;;; $Id: rule-patterns.lisp,v 1.5 2008/01/23 08:57:19 amallavarapu Exp $
;;;
(in-package b)

(defstruct (rule-parse (:conc-name rule-parse-)
                       (:constructor _make-rule-parse (patterns 
                                                      &aux 
                                                      (user-pvars (extract-pattern-vars patterns))
                                                      (pvars ()))))
  patterns classes user-pvars pvars lisa-patterns)

(defconstant *logical-pattern-ops* '(:and :or :not :exists :find))

(define-condition pattern-var-type-mismatch (type-error) ())

(defun make-rule-parse (patterns)
  (let* ((rp (_make-rule-parse patterns))
         (lisa-patterns (rule-parse-add-pattern rp patterns)))
    (setf (rule-parse-lisa-patterns rp) lisa-patterns)
    rp))

(defun rule-parse-add-pattern (rule-parse pattern &optional (rule-parse-bodies '(())))
  "Adds a new pattern to a list of rule-parse-bodies, returning a new list of rule-parse-bodies"
  (labels ((add-and-patterns (subp rule-parse-bodies)
             (if subp 
                 (add-and-patterns (cdr subp) 
                                   (rule-parse-add-pattern rule-parse (car subp) 
                                                     rule-parse-bodies))
               rule-parse-bodies))
           
           ;; :NOT - Each non-existent pattern produces an independent rule-parse
           ;;        (This means the rule-parse body may be fired multiple times for
           ;;        each satisfaction of the NOT clause)
           (add-not-patterns (subp)
             (let ((not-patterns (mapcar (lambda (p)
                                           (if (eq (car p) 'lisa:test)
                                               `(lisa:test (not ,@(cdr p)))
                                             `(not ,p)))
                                         (mapcan (lambda (p) (rule-parse-read-value-pattern rule-parse p)) subp))))
               (mapcan (lambda (rb)
                         (mapcar (lambda (np) ;; produces a list of rule-parse-bodies
                                   (append rb (list np))) ;; one with each not predicated pattern
                                 not-patterns))
                       rule-parse-bodies)))
           ;; :EXISTS
           (add-exists-patterns (subp)
             (let ((exists-patterns (mapcan (lambda (p) (rule-parse-read-value-pattern rule-parse p)) subp)))
               (mapcar (lambda (rb)
                         (append rb 
                                 (mapcar (lambda (p)
                                           `(lisa:exists ,p))
                                         exists-patterns)))
                       rule-parse-bodies)))

           (extend-rule-parse-bodies (lisa-patterns)
             (mapcar (lambda (rb) (append rb lisa-patterns)) rule-parse-bodies)))

    (let ((pattern (or (expand-pattern pattern nil) pattern)))
      (case (pattern-type pattern)
        (:value  (extend-rule-parse-bodies (rule-parse-read-value-pattern rule-parse pattern)))
      
        (:test   (extend-rule-parse-bodies `((lisa:test ,@(test-pattern-form pattern)))))

        (:not    (add-not-patterns (pattern-args pattern)))

        (:exists (add-exists-patterns (pattern-args pattern)))
      
        (:and    (add-and-patterns (pattern-args pattern) rule-parse-bodies))

        (:or     (mapcan (lambda (subp)                         
                           (if subp (rule-parse-add-pattern rule-parse subp rule-parse-bodies)
                             rule-parse-bodies)) ;; NIL - sub patterns cause duplication of the rule-parse body
                         ;; without adding a pattern - this is useful for detecting null conditions
                         (pattern-args pattern)))

        (:find   (let ((find-pattern (pattern-args pattern))) ;; :find always succeeds, but pattern-vars
                   (rule-parse-add-pattern rule-parse `(:or (:not ,@find-pattern) 
                                                        (:and ,@find-pattern))
                                           rule-parse-bodies)))

        (:global (add-global-pattern rule-parse pattern))

        (otherwise (b-error "unrecognized pattern type."))))))

(defun add-global-pattern (rule-parse p &optional (obj-var (gensym "?OBJ") obj-var-p))
  (let ((o (etypecase p
             (symbol   (global-value p))
             (fld-form (cclass-property (global-value (fld-form-object p))
                                        (fld-form-field p))))))
    (etypecase o
      (concept         (let ((class (class-of o)))
                         (rule-parse-add-class rule-parse class)
                         (if obj-var-p `((lisa:test (eq ,obj-var ,o)))
                           `((,(class-name (class-of o)) (:object ,obj-var))
                             (lisa:test (eq ,obj-var ,o))))))
      (concept-class   (rule-parse-add-class rule-parse o)
                       `((,(class-name o) (:object ,obj-var))))
      (property-class  (rule-parse-add-class rule-parse o)
                       `((,(class-name o) (object (gensym "?CONCEPT")) (value ,obj-var)))))))

(defun pattern-var (s &optional (package *package*))
  "Prepends ? to the symbol s, and returns a var in the current package"
  (intern (mkstr "?" s) package))

(defun make-regular-symbol-from-pattern-var (s &optional (package *package*))
  (intern (subseq (symbol-name s) 1)
          (if package package (symbol-package s))))

(defun pattern-var-p (s)  
  (and (symbolp s)
       (eq (char (symbol-name s) 0)
           #\?)
       (#!eq s ?)))

(deftype pattern-var ()
  `(satisfies pattern-var-p))
         
(defun pattern-head (p)
  (car p))

(defun pattern-args (p)
  (cdr p))

;;;; (defun func-pattern-p (p)
;;;;   (and (consp p)
;;;;        (or (symbolp (first p))
;;;;            (lambda-expr-p (first p)))))

(defun lambda-expr-p (o)
  (and (consp o)
       (eq (first o) 'lambda)
       (consp (second o))))


(defun test-pattern-p (p)
  (and (consp p) (eq (first p) :test)))

(defun test-pattern-form (p) (rest p))

(defun pattern-type (p)
  (cond
   ((test-pattern-p p)
    :test)

   ((globalp p)
    :global)

   ((logic-pattern-p p)
    (logic-pattern-op p))

   ((value-pattern-p p)
    :value)

;;;;    ((func-pattern-p p)
;;;;     :func)

   (t (b-error "Invalid pattern ~A" p nil))))

(defun logic-pattern-p (p &optional (type nil type-present))
  "True if of type (OP SUB-PATTERNS) where OP = :AND, :OR, :NOT, or :EXISTS"
  (if (and (consp p)        
           (cond (type-present (eq p type))
                 (t            (member (car p) 
                                   *logical-pattern-ops*))))
      t))

(defun logic-pattern-op (p)
  (car p))

(defun obj-bind-pattern-p (p)
  "True if expression is of type (?VAR [...])"
  (if (and (consp p)
           (pattern-var-p (first p))
           (object-form-p (second p))) t))

(defun obj-bind-pattern-var (p)
  (first p))

(defun obj-bind-pattern-pattern (p)
  (second p))

;;; PVAR TYPE Tracking: 
(defun rule-parse-add-pvar-type (rule-parse pvar type)
  (symbol-macrolet ((pvars (rule-parse-pvars rule-parse)))
    (when type
      (let* ((existing (assoc pvar pvars))
             (existing-type (cdr existing)))
        (if existing-type
            (cond ((subtypep (cdr existing) type) 
                   existing-type)
                  ((subtypep type existing-type)
                   (setf (cdr existing) type))
                  (t (b-error "attempt to bind ~A to incompatible types (~S ~S)"
                              pvar existing-type type)))
          (push (cons pvar type) pvars))))))
  
(defun rule-parse-pvar-type (rule-parse pvar)
  (let ((type (assoc-val pvar (rule-parse-pvars rule-parse))))
    (if type type
      (b-error "type of ~A is unknown" pvar))))

;;; class-tracking
(defun rule-parse-add-class (rule-parse class)
  (when (member class (list property concept))
    (b-error "Cannot pattern match against ~S." class))
  (pushnew class (rule-parse-classes rule-parse)))

;;;
;;; VALUE patterns - can be bound to a pattern var :=
;;;                  obj-pattern = [...]
;;;                  fld-pattern = O.F1.F2..Fn
;;;                  varbind patterns = (?VAR {obj-pattern | fld-pattern}*)
;;;
(defun rule-parse-read-value-pattern (rule-parse p &optional (val-var (gentemp "?O")))
  (let ((p (or (expand-pattern p val-var) p)))
    (cond 
     ((symbolp p)
      (add-global-pattern rule-parse p val-var))
   
     ((fld-form-p p)
      (rule-parse-read-fld-pattern rule-parse p val-var))
   
     ((object-form-p p)
      (rule-parse-read-object-pattern rule-parse p val-var))
   
     ((varbind-pattern-p p)
      (mapcan (lambda (subp)
                (let ((epattern (expand-pattern subp (varbind-pattern-var p))))
                  (if epattern
                      (mapcan (lambda (esubp)
                                (apply #'append (rule-parse-add-pattern  rule-parse esubp)))
                              epattern)
                    (rule-parse-read-value-pattern rule-parse subp (varbind-pattern-var p)))))
              (varbind-pattern-args p)))
       
     (t (b-error "Pattern ~S is unrecognized.  "
                 p)))))



;;;
;;; FLD patterns - O.F1.F2...Fn.  Where value-var is a pattern-var which will bind the value of the field
;;;                e.g., in (?V O.F), ?V is the value-var
;;;              - or property patterns: CLASS.PROPERTY
(defun rule-parse-read-fld-pattern (rule-parse fe value-var)
  (let* ((obj         (fld-form-object fe))
         (fld         (compute-form-value (fld-form-field fe) nil "Field expression patterns must have constant fields"))
         (class-type  (if (pattern-var-p obj) (rule-parse-pvar-type rule-parse obj)
                        (b-compute-form-type (fld-form-object fe) nil t))))
    
    (assert class-type ()
      "Unable to determine type of ~S." obj)
        
    ;(assert (null (fld-form-args fe)) ()
    ;  "Arguments not allowed in field expression patterns: ~S" fe)
    
    (cond 
     ((eq class-type 'concept-class)
      (let ((class (cclass-property (find-class obj) fld)))
        (rule-parse-add-class rule-parse class)
        `((,(class-name class) 
           (object ,(gentemp "?OBJ")) (value ,value-var)))))

     ((subtypep class-type 'concept)
      (let ((class (or (find-class class-type)
                       (b-error "~S is not a valid class in pattern ~S" obj fe))))

        (case (fieldinfo-kind (class-fieldinfo class fld))
          (:property        
           (let+ ((pclass                       (cclass-property class fld))
                  (pclass-name                  (class-name pclass))
                  ((obj-pattern extra-patterns) (rule-parse-read-slot-pattern rule-parse 'object obj class-type)))
             (rule-parse-add-class rule-parse pclass)
             `((,pclass-name ,obj-pattern (value ,value-var))
               ,@extra-patterns)))
      
          (:id
           (let+ (((obj-pattern extra-patterns) (rule-parse-read-slot-pattern rule-parse :object obj class-type)))
              (rule-parse-add-class rule-parse (find-class class-type))
              `((,class-type (,fld ,value-var) 
                             ,obj-pattern)
                ,@extra-patterns)))

          (:method
           (rule-parse-read-fld-method-pattern rule-parse fe value-var)))))

      (t (error "Invalid pattern: ~S.~A - ~S is not a concept or concept-class."
                obj fld obj)))))

;;;
;;; OBJECT PATTERN:  [...] or (?VAR [...]) - returns a list of LISA patterns
;;; 
(defun rule-parse-read-object-pattern (rule-parse pattern &optional (obj-var (gentemp "?O")))
  "Converts expressions into a list of LISA patterns.  read-object-pattern is called by :and :or :not :exists pattern readers and dispatches to more specific object pattern expression readers."
  (cond ((pattern-var-p (car pattern))
         (rule-parse-read-object-pattern rule-parse (second pattern) (first pattern)))
        
        ((object-form-p pattern)
         (let* ((head (object-form-object pattern)))
           (cond
            ((symbolp head)
             (let ((gval  (global-value head)))
               (typecase gval
                 ;; CCLASS-PATTERN:  [CCLASS id-fields]
                 (concept-class     (rule-parse-read-cclass-pattern rule-parse pattern obj-var))
                  
                 ;; CONCEPT-SYMBOL-PATTERN:  [CONCEPT prop-fields]
                 (concept           (rule-parse-read-concept-symbol-pattern rule-parse pattern obj-var))

;;;;                  ;; INTERFACE-CLASS-PATTERN: [ICLASS obj]
;;;;                  (interface-class   (b-error "Use [[ICLASS obj] INTERFACE-FIELDS]"))

                 ;; PROPERTY-CLASS-PATTERN: [PROPERTY-CLASS obj 
                 (property-class    (rule-parse-read-property-class-pattern rule-parse pattern obj-var))
                   
                 (t                 (b-error "~S is not a concept class or object" head)))))

            ((fld-form-p head)
             (rule-parse-read-property-class-pattern rule-parse pattern obj-var))
            
            ((object-form-p head)
             (let ((gval (global-value (object-form-object head))))
               (typecase gval                 
                 ;; CONCEPT-PATTERN [[cclass id-fields] ...prop-fields...]
                 (concept-class          (rule-parse-read-concept-pattern rule-parse pattern obj-var))
                 
;;;;                  ;; INTERFACE-PATTERN [[iclass obj] ...interface-fields...]
;;;;                  (interface-class        (rule-parse-read-interface-pattern rule-parse pattern obj-var))

                 (t                      (b-error "~S is not an interface class")))))
            
            (t (b-error "invalid object pattern: ~A." pattern nil)))))))
 


;;;; ;;
;; CONCEPT SYMBOL PATTERN = [ CONCEPT-SYMBOL PROPERTY-FIELDS ]
;; - returns a list of LISA patterns
(defun rule-parse-read-concept-symbol-pattern (rule-parse pattern &optional (obj-var (gentemp "?O")))
  "Converts object expressions of the form: [CONCEPT-SYMBOL PROPERTIES].  Called by read-object-patterns."
  (let* ((concept-symbol (pattern-head pattern))
         (prop-args      (pattern-args pattern))
         (object         (global-value concept-symbol))
         (cclass-symbol  (type-of object))
         (cclass         (class-of object))
         (prop-patterns  (rule-parse-read-properties rule-parse cclass prop-args obj-var)))
    (rule-parse-add-pvar-type rule-parse obj-var cclass-symbol)
    `(,@prop-patterns
      (,cclass-symbol (:object ,obj-var))
      (lisa:test (eq ,obj-var ,concept-symbol)))))

;;
;; CONCEPT PATTERN = [CCLASS-PATTERN PROPERTY-FIELDS]
;; - returns a list of LISA patterns
(defun rule-parse-read-concept-pattern (rule-parse object-form obj-var)
  "Reads a pattern of the form [[CCLASS ID-ARGS] PROP-ARGS] produces a list of LISA patterns."
  (let* ((cclass-pattern  (object-form-object object-form))
         (prop-args       (object-form-args object-form))
         (cclass-symbol   (object-form-object cclass-pattern)))
    `(,@(rule-parse-read-properties rule-parse (find-class cclass-symbol) prop-args obj-var)
      ,@(rule-parse-read-cclass-pattern rule-parse cclass-pattern obj-var))))
                     
;;
;; CCLASS PATTERN = [CCLASS ID-FIELDS]
;; - returns a list of LISA patterns
(defun rule-parse-read-cclass-pattern (rule-parse object-form obj-var)
  "Reads a pattern of the form [CCLASS ... ID-FIELDS...] and produces a list of LISA patterns"
  (let* ((cclass-symbol (object-form-object object-form))
         (id-args       (object-form-args object-form))
         (bindings      (cclass-read-pattern-args cclass-symbol id-args))
         (sub-patterns  ())
         (slot-bindings ()))
    (declare (special *exact-cclasses*))

    (rule-parse-add-pvar-type rule-parse obj-var cclass-symbol)
    (rule-parse-add-class rule-parse (find-class cclass-symbol))
    ;; *EXACT-CCLASSES* is a dynamic var holding a list of classes which
    ;; which should be matched exactly.  I.e., sub-classes of an exact cclass are
    ;; not matched.     
    (if (and (boundp '*exact-cclasses*) 
             (member cclass-symbol *exact-cclasses*))
        (push `(lisa:test (eq (type-of ,obj-var) ',cclass-symbol))
              sub-patterns))

    (mapcar (lambda (binding)             
              (multiple-value-bind (new-slot-binding new-tests)
                  (rule-parse-read-slot-pattern rule-parse
                                          (car binding)
                                          (cdr binding)
                                          (ignore-errors (fieldinfo-type (class-fieldinfo (global-value cclass-symbol)
                                                                                          (car binding)))))
;                                  (b-compute-form-type `(fld ,cclass-symbol ,(car binding)) nil nil))
                                  ;;(get-field-type cclass-symbol (car binding)))
                                  (push new-slot-binding slot-bindings)
                                  (setf sub-patterns (nconc sub-patterns new-tests))))
            bindings)
    
    `((,cclass-symbol ,@slot-bindings (:object ,obj-var))
      ,@sub-patterns)))


;;;; ;;
;;;; ;; INTERFACE PATTERN = [[ICLASS OBJ] INTERFACE-FIELDS]
;;;; ;; - returns a list of LISA patterns
;;;; (defun rule-parse-read-interface-pattern (rule-parse object-form obj-var)
;;;;   (let* ((iclass-pattern  (object-form-object object-form))
;;;;          (iclass-symbol   (object-form-object iclass-pattern))
;;;;          (iclass          (global-value iclass-symbol))
;;;;          (obj-arg         (first (object-form-args iclass-pattern)))
;;;;          (obj-binding     ())
;;;;          (bindings        (iclass-read-pattern-args iclass (object-form-args object-form))) ;; interface fields
;;;;          (sub-patterns    ())
;;;;          (slot-bindings   ()))
;;;;          
;;;;     (when obj-arg
;;;;       (let ((obj-fld (multiple-value-list (rule-parse-read-slot-pattern rule-parse 'object obj-arg  t))))
;;;;         (setf obj-binding (first obj-fld)
;;;;               sub-patterns (second obj-fld))))
;;;;     
;;;;     (when obj-var
;;;;       (rule-parse-add-pvar-type rule-parse obj-var iclass-symbol))

;;;;     (mapcar (lambda (binding)
;;;;               (multiple-value-bind (new-slot-binding new-tests)
;;;;                   (rule-parse-read-slot-pattern rule-parse
;;;;                                           (car binding)
;;;;                                           (cdr binding)
;;;;                                           (get-field-type iclass (car binding)))
;;;;                 (push new-slot-binding slot-bindings)
;;;;                 (setf sub-patterns (nconc sub-patterns new-tests))))
;;;;             bindings)

;;;;       `((,iclass-symbol ,@(when obj-binding `(,obj-binding))
;;;;                         ,@slot-bindings 
;;;;                         ,@(if obj-var `((:object ,obj-var))))
;;;;         ,@sub-patterns)))

;;
;; PROPERTIES - [[CCLASS ..] :PROP1 VAL1 [:PROP2 [CCLASS2] :RELEVANCE 1] ... 
;;               => 
;;              ((CCLASS (..) (:OBJECT ?O123))
;;               (CCLASS.PROP1 (OBJECT ?O123) (VALUE VAL))   \_ read-properties produces default-context
;;               (CCLASS.PROP2 (OBJECT ?O123) (VALUE ?V124)) /
;;               (CCLASS2 (:OBJECT ?V124)))
;;
(defun rule-parse-read-properties (rule-parse cclass plist obj-var)
  "Process the properties in [ CONCEPT PROPERTIES] and returns a list of LISA patterns"
  (let ((patterns  ()))

    (labels ((new-property-pattern (field val &optional (rel nil relp))
               (let+ ((pclass     (cclass-property cclass field))
                      (pclass-sym (class-name pclass))
                      (type       (pclass-type pclass))
                      ((value-binding value-patterns) (rule-parse-read-slot-pattern rule-parse 'value val type)))
                   ;; (CCLASS.PROPERTY (OBJECT ?OBJ-VAR) (VALUE ...))
                   (rule-parse-add-class rule-parse (find-class pclass-sym))
                   (push `(,pclass-sym
                           (object ,obj-var)
                           ,value-binding
                           ,@(if relp `((:relevance ,rel))))
                         patterns)
;;
                   (setf patterns (nconc patterns value-patterns))))
             
             (process-property (plist)
               (let ((kw  (first plist))
                     (val (second plist)))
                 
                 (cond ;; :PROP VAL form
                       ((keywordp kw)
                        (new-property-pattern kw val)
                        (cddr plist))
                       
                       ;; [:PROP VAL ..:RELEVANCE] form
                       ((and (object-form-p kw)
                             (keywordp (object-form-object kw)))
                        
                        (let* ((args (object-form-args kw))
                               (val  (first args))
                               (has-rel (if (eq (second args) :relevance) t))
                               (rel  (if has-rel (third args))))
                          (if has-rel
                              (new-property-pattern (object-form-object kw) val rel)
                            (new-property-pattern (object-form-object kw) val)))
                        (cdr plist))
                       
                       (t (b-error "invalid property pattern for class ~A with ~A"
                                   (car plist)
                                   (cdr plist)))))))
      
      (loop with iter = plist
            while (setf iter (process-property iter)))
      patterns)))

;;
;; 
(defun rule-parse-read-slot-pattern (rule-parse field pattern type)
  "Returns two values: (read-slot-pattern field pattern) =>
                     value-0 = lisa slot pattern (:FIELD ?VAR), 
                     value-1 = new patterns which bind ?VAR 
                     (new patternsmay be NIL if pattern is a simple value
                      like a number, string or pattern-var)"

  (macrolet ((subst-field-var (new-patterns)
               `(let ((?field-var (gentemp "?FVAR")))
                  (rule-parse-add-pvar-type rule-parse ?field-var type)
                  (values `(,field ,?field-var)
                          ,new-patterns))))

    (flet ((field-pattern-error ()
             (b-error "invalid pattern (~S) for field ~A"
                      pattern field))
           (type-check (o)
             (if (and type
                      (not (subtypep (type-of o) type)))
                 (b-error "~A" (make-condition 'pattern-var-type-mismatch
                                               :datum field :expected-type type)))))
    
      (cond ((pattern-var-p pattern) ; :FIELD ?VAR => (_:FIELD ?VAR)
             (rule-parse-add-pvar-type rule-parse pattern type)
             `(,field ,pattern))

            ((varbind-pattern-p pattern) ; special handling for varbind patterns. i.e, :slot (?var ...pattern)
             (let ((var     (varbind-pattern-var pattern)))
               (values `(,field ,var)
                       (rule-parse-read-value-pattern rule-parse pattern var))))

            ((value-pattern-p pattern)
             (subst-field-var 
              (rule-parse-read-value-pattern rule-parse pattern ?field-var)))

            ((null pattern)
             (type-check (type-of pattern))
             (subst-field-var `((lisa:test (null ,?field-var)))))

            ((typep pattern '(or number vector array (and symbol (satisfies constantp))))
             (type-check (type-of pattern))
             `(,field ,pattern))

            ;((constantp pattern)
            ; (subst-field-var `((lisa:test (eq ,?field-var ,pattern)))))
            ((constantp pattern) `(,field ,pattern))
            
            ((consp pattern)       
             (case (car pattern)  
               (quote              ; :FIELD 'OBJ or :FIELD '(...)
                (let ((qval (second pattern))) ; quoted value
                  (cond ((consp qval)
                         (subst-field-var `((eq ,?field-var ',qval))))
                        
                        (t `(,field ,qval)))))
               
               (otherwise          ; :FIELD FUNCTION-FORM
                (subst-field-var `((lisa:test (eq ,?field-var ,pattern)))))))
            
            (t (field-pattern-error))))))


(defun extract-pattern-vars (pattern)
  "Finds all occurrences of pattern variables"
  (let ((pvars nil))
    (maptree (lambda (o) 
               (if (pattern-var-p o)
                   (setf pvars (adjoin o pvars))))
             pattern)
    (nreverse pvars)))


;;;
;;; VALUE-PATTERN - A pattern which has a value
;;;
(defun value-pattern-p (p)
  (or (varbind-pattern-p p)
      (globalp p)
      (fld-form-p p)
      (object-form-p p)))


;;;
;;; VARBIND PATTERN - (?VAR composite-pattern)
;;;
(defun varbind-pattern-p (p)
  "(?PATTERNVAR ...) Binds the value of the remaining expressions to PATTERN-VAR"
  (if (and (consp p)
           (pattern-var-p (first p)))
      t))

(defun varbind-pattern-var (p)
  (first p))

(defun varbind-pattern-args (p)
  (cdr p))

;;;
;;; COMPOSITE-PATTERN - list of object-form patterns or fld-form patterns
;;;
(defun composite-pattern-p (p)  
  (and (consp p)
       (every (lambda (arg)
                (or (object-form-p arg) (fld-form-p arg)
                    (symbolp arg)))
              p)))



;;;;
;;;; [PROPERTY-CLASS OBJECT PROP-VALUE RELEVANCE] where prop-value and relevance are optional
;;;;
(defun rule-parse-read-property-class-pattern (rule-parse pattern &optional (obj-var (gentemp "?O")))
  (declare (ignorable rule-parse pattern obj-var))
  (destructuring-bind (object-symbol pclass-form object &optional (value (gensym "?V")) (relevance (gensym "?R")))
      pattern
    (assert (eq object-symbol 'object))
    (let+ ((pclass                      (if (fld-form-p pattern)
                                            (fld (global-value (fld-form-object pclass-form))
                                                 (fld-form-field pclass-form))
                                          (global-value pclass-form)))
           ((value-binding value-tests) (rule-parse-read-slot-pattern rule-parse
                                                                'value
                                                                value
                                                                (pclass-type pclass))))
      (assert (typep pclass 'property-class) () "Expecting a property class, but received ~S" pclass)
      (rule-parse-add-class rule-parse pclass)
      `((,(class-name pclass) (object ,object) 
                              ,value-binding
                              (relevance ,relevance) 
                              (:object ,obj-var))
        ,@value-tests))))

;;;;;
;;;;;
;;;;;  RHS handling:
;;;;;

(defun rule-parse-rhs-lambda-list (rule-parse)
  "Returns a binary list holding the appropriate lambda-list and declares for the rhs-fn of RULE.  USAGE: `(LAMBDA ,@(rule-parse-rhs-lambda-list rp) ,@rhs-code), where rp = a rule-parse object, RHS-CODE = forms representing rhs-code"
  (let* ((user-pvars                (rule-parse-user-pvars rule-parse)))
    `((&key ,@user-pvars &allow-other-keys) ;; (&key (?v1 ?) ...)
      (declare (ignorable ,@user-pvars)))))

(defun rule-parse-rhs-apply-args (rule-parse)
  (mapcan (lambda (v) (list (key v) v)) (rule-parse-user-pvars rule-parse)))

(defun rule-parse-lisa-rule-definitions (rule-parse name code)
  "Patterns = LISA-PATTERNS"
  (flet ((build-lisa-rule (num lisa-pattern)
           (let* ((contains-not (find-if (with #'eq 'not) lisa-pattern :key #'car))
                  (number-name  (if name (sym name "LISA-RULE-" num) (make-symbol (mkstr "LISA-RULE-" num)))))
             `(lisa:defrule ,number-name (:salience ,(if contains-not 0 1)) ;; not rules run last
                ,@lisa-pattern
                lisa:=>
                (kb-transaction ,code)))))
    (loop for pattern in (rule-parse-lisa-patterns rule-parse)
          for rule-num = 1 then (1+ rule-num)
          collect (build-lisa-rule rule-num pattern))))

(defun add-rule (pattern function)
  (dolist (lisa-pattern (rule-parse-lisa-patterns (make-rule-parse pattern)))
    (lisa::define-rule (gensym) (list lisa-pattern 'lisa:=> function))))


;;;
;;; pattern expanders
;;;
(defvar *pattern-expanders* ())

(defun add-pattern-expander (name &optional (priority 0))
  (setf *pattern-expanders* (add-prioritized-symbol name priority *pattern-expanders*)))

(defun remove-pattern-expander (name)
  (setf *pattern-expanders* (remove-prioritized-symbol name *pattern-expanders*)))

(defun expand-pattern (p objvar &optional (expanders *pattern-expanders*))
  (if expanders
      (or (funcall (caar expanders) p objvar)
          (expand-pattern p objvar (rest expanders)))))
  