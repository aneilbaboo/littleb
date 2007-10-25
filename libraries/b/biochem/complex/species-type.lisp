;;; The MIT License

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

;;; $Id: species-type.lisp,v 1.8 2007/10/25 20:12:52 amallavarapu Exp $
;;; $Name:  $

;;; File: complex-species-type.lisp
;;; Description: defines the complex-species-type structure
;;;              relies on the graph-tools library

;;;
;;; examples:
;;;    a species: [[ksr 1 _ _] [mapk 1 :u]] ; shorthand = [[ksr 1][mapk 1]]
;;;    a reaction: {[ksr _ * *] + [mapk _] ->> [[ksr 1][mapk 1]]}

;;;
;;; SITE - describes a site - 
;;; MONOMER - a description of a fragment of a graph
;;; COMPLEX - a species-type which 
;;;
(in-package #I@library/biochem)

(include b/biochem/species)

;;;
;;;
(define-macro defmonomer (name-lclass &body def)
  (mutils:let+ (((symbol lclass) (etypecase name-lclass
                                   (symbol (values name-lclass 'compartment))
                                   (cons   (values (first name-lclass) (second name-lclass)))))
                ((doc sites) (if (stringp #1=(first (last def))) (values #1# (butlast def))
                               (values nil def))))
    `(port:dspec (defmonomer ,symbol)
       (define ,symbol [[monomer] 
                        :location-class ,lclass
                        :sites ',sites
                       ,@(if doc `(:documentation ,doc))])
       ',symbol)))

;;;
;;; MONOMER concept - main role is to hold sites
;;;
(defcon monomer ()
  (&optional (name := *name*)
   &property (location-class := compartment)))

(defprop monomer.sites ()
 (let* ((sindex -1)
        (sites (map 'vector (lambda (sspec)
                              (apply #'make-site-info (append (if (listp sspec) sspec (list sspec))
                                                              (list :index (incf sindex)))))
                    value))
        (slist (coerce sites 'list)))
   (when #1=(set-difference slist
                            (remove-duplicates slist :key #'site-info-name))
     (b-error "Duplicate site name (~S) in ~S" object 
              (site-info-name (first #1#))))
   sites))
     

(defield monomer.site (x)
  (etypecase x
    (integer (svref .sites x))
    (symbol  (find x .sites :key #'site-info-name))))

(defgeneric monomer-lclass (m)
  (:method ((m symbol)) (|MONOMER.LOCATION-CLASS| (eval m)))
  (:method ((m monomer)) (|MONOMER.LOCATION-CLASS| m))
  (:method ((m cons)) (|MONOMER.LOCATION-CLASS| (eval (fld-form-object m)))))

(defgeneric monomer-sites (m)
  (:method ((m symbol)) (|MONOMER.SITES| (eval m)))
  (:method ((m monomer)) (|MONOMER.SITES| m))
  (:method ((m cons)) (monomer-sites (fld-form-object m))))

(defun monomer-site-info (m i)
  (svref (monomer-sites m) i))

(defmethod documentation ((d monomer) (type (eql :ctor-help)))
  (labels ((simplify-label (si) 
             (let ((labs (site-info-tags si)))
               (if (= (length labs) 1) (First labs) labs)))
           (state-site-doc (si)
             (list (simplify-label si)
                   (state-site-info-type si))))
    (format nil "COMPLEX CTOR: [~S~{ ~S~}]~@[ / CONNECTORS: ~{~S~^, ~}~]~@[ / VALUES: ~{~{~S.[~S]~}~^, ~}~]~@[ -- ~A~]"
            d.name
            (map 'list #'simplify-label d.sites)
            (map 'list #'site-info-name (remove-if-not #'bond-site-info-p d.sites))
            (map 'list #'state-site-doc (remove-if-not #'state-site-info-p d.sites))
            d.documentation)))

(defun monomer-symbol-p (o)
  (or (wildcard-monomer-symbol-p o)
      (and (symbolp o)       
           (monomer-p (ignore-errors (eval o))))))

(predefine (:global complex-species-type)
(defmethod documentation ((d monomer) (type (eql :fields-help)))
  (documentation complex-species-type :fields-help)))

(port:define-dspec-class defsite () "")
(port:define-dspec-class defmonomer () "")
(port:define-dspec-form-parser defmonomer (name)
  (if (consp name) `(defmonomer ,(first name))
    `(defmonomer ,name)))

;;;
;;; SITE-INFO structure
;;;
(defstruct (site-info (:constructor nil)) tags index)

(defun site-info-name (s) (first (site-info-tags s)))

;; bond sites can connect to other bond sites
(defstruct (bond-site-info (:include site-info))
  type sublocation)

;; state sites have values
(defstruct (state-site-info (:include site-info))
  type default)

(defun member-type-p (x) (and (consp x) (eq (first x) 'member)))
(defun keywordify (x)
  (typecase x
    (symbol (intern (symbol-name x) :keyword))
    (t      x)))

(defun make-site-info (tags &rest args &key value default connector sublocation index)
  (declare (ignorable args))
  (let* ((tags      (mapcar #'keywordify
                            (if (listp tags) tags (list tags))))
         (name      (first tags))
         (connector (or connector (not (or value default))))
         (value     (or value (and (not connector) (not sublocation))))
         (default    (or default 
                        (and (member-type-p value)
                             (second value)))))
    (cond

     ;; attempt to provide both connector and value definitions:
     ((and value connector)
      (error "Ambiguous site definition ~S - value site or a connector site?"))

     ;; when it's a not a connector, check default value is valid
     ((not (or connector
               (typep default value)))
      (error "Default value ~S is not of type ~S in site ~S"
             default value name))

    
     (value (make-state-site-info :tags tags :type value :default default :index index))

     (t     (make-bond-site-info :tags tags
                                 :index index
                                 :type connector
                                 :sublocation sublocation)))))


(defun ensure-canonical-complex-graph (x &optional force-pattern)
  (gtools:canonical-graph
   (etypecase x
     (list (make-complex-graph x force-pattern))
     (complex-graph x))))

;;;
;;; COMPLEX GRAPH - graph structure
;;;
(defstruct (complex-graph (:constructor %make-complex-graph)
                          (:include gtools:labelled-graph)))

(defmethod gtools:graph-type-label-less-predicate ((type (eql 'complex-graph)))
  (labels ((complex-graph-label< (x y)
              (etypecase x
               (symbol
                (etypecase y 
                  (symbol (if (eq x y)
                              (string< (package-name (symbol-package x))
                                       (package-name (symbol-package y)))
                            (string< (symbol-name x) (symbol-name y))))
                  (fld-form t)
                  (number nil)
                  (cons   t)))
               (fld-form
                (etypecase y
                  (fld-form (let ((xobj (fld-form-object x))
                                  (yobj (fld-form-object y))
                                  (xfld (fld-form-field x))
                                  (yfld (fld-form-field y)))
                              (or (complex-graph-label< xobj yobj)
                                  (and (eq x y)
                                       (complex-graph-label< xfld yfld)))))
                  (cons t)
                  (number t)
                  (symbol nil)))
               (number   (etypecase y
                           (number (< x y))
                           (fld-form t)
                           (symbol t)
                           (cons   t)))
               (cons
                (etypecase y
                  (symbol nil)
                  (number nil)
                  (fld-form nil)
                  (cons   (or (and (eq (first x) (first y))
                                   (complex-graph-label< (second x) (second y)))
                              (complex-graph-label< (first x) (first y)))))))))
    #'complex-graph-label<))

(defmethod gtools:graph-type-label-test-predicate ((type (eql 'complex-graph)))
  (labels ((site-test (test y)
             (etypecase test
               (null t)
               (atom (typecase y
                       (atom (eql test y))
                       (list (find test y :test #'equal))))
               (cons
                (let ((test-labs (rest test)))
                  (ecase (first test)
                    (AND   (null (set-difference test-labs y
                                                 :test #'logical-test)))
                    (OR    (some (lambda (lab)
                                   (member lab test-labs))
                                 y :test #'equal))
                    (NOT   (not (site-test `(or ,@test-labs) y))))))))

           (value-test (test value)
             (typecase test
               ((eql *) t)
               (atom    (eql test value))
               (cons    (find value test :test #'eql))))

           (wildcard-test (test glabel)
             (let ((site-test (wildcard-label-tags test))
                   (value-test (wildcard-label-state test)))
               (and (site-test site-test 
                               (complex-graph-label-to-site-labels glabel))
                    (value-test value-test (site-label-value glabel)))))

           (complex-graph-label-test-predicate (test glabel)
             (cond
               ((symbolp test)
                (or (and (eq test '?) (symbolp glabel))
                    (eq test glabel)))

               ((wildcard-label-p test)
                (wildcard-test test glabel))

               ((site-label-p test)
                (and (site-label-p glabel)
                     (equal (site-label-sindex test) (site-label-sindex glabel))
                     (equal (site-label-monomer test) (site-label-monomer glabel))
                     (value-test (site-label-value test) (site-label-value glabel))))

               (t (error "Unexpected test graph label:~S" test)))))
    #'complex-graph-label-test-predicate))

;;;
;;;  BASE CLASS: COMPLEX-GRAPH-CONCEPT
;;;
(defcon complex-graph-concept (:abstract)
  (&optional id 
   &property location-class))
  
(defun complex-graph-location-class (cg monomer-test)
  (loop with lclass = compartment
        with lclass-dim = *compartment-dimensionality*
        for lab across (gtools:graph-labels cg)
        when (funcall monomer-test lab)
        do (let* ((m-lclass (monomer-lclass lab))
                  (m-dim    (location-class-dimensionality m-lclass)))
             (when (< m-dim lclass-dim)
               (setf lclass-dim m-dim
                     lclass m-lclass)))
        finally return lclass))
              
        

(defprop complex-graph-concept.constructor-description
    (:=
     (complex-graph-description object.id)))

(defmethod print-concept ((o complex-graph-concept) &optional stream)
  (if *debug-printing* (call-next-method)
    (let* ((descr o.constructor-description)
           (complex (> (length descr) 1)))
      (pprint-logical-block (stream descr
                                    :prefix (if complex "[" "")
                                    :suffix (if complex "]" ""))
        (loop for mdescr = (pprint-pop)
              do (prin1 `[,@mdescr] stream)
              (pprint-newline-selectively :linear stream)
              (pprint-exit-if-list-exhausted))))))


;;;
;;;  COMPLEX-SPECIES-TYPE:
;;;
;;;
(defcon complex-species-type (species-type complex-graph-concept)
  "SPECIES-TYPE which is described as a graph"
  (&optional (id 
              :documentation "A list specifying monomers and their connectivity"))
  (etypecase id
    (complex-graph nil)
    (cons          (setf .id (gtools:canonical-graph 
                              (etypecase id
                                (complex-graph  id)
                                (cons           (make-complex-graph id nil)))))))
  =>
  (setf .location-class (complex-graph-location-class .id #'monomer-symbol-p)))

(defield complex-species-type.monomers ()
  (remove-if-not #'symbolp (gtools:graph-labels .id)))

(defield complex-species-type.site-bonds (mindex sindex)
  (complex-graph-site-bonds .id mindex sindex))


;;; REFERENCE-PATTERN
(defcon reference-pattern (complex-graph-concept :notrace)
  "Reference patterns are created when reactions are defined; each monomer has a reference number indicating its identity in the context of a complex-reaction-type"
  (id)
  (setf .id (ensure-canonical-complex-graph id t))
  =>
  (setf .location-class (complex-graph-location-class .id #'monomer-symbol-ref-p)))



;;;
;;; COMPLEX-PATTERN
;;;
(defcon complex-pattern (reference-pattern :notrace)
  "Selector patterns are asserted into the database and detected by the detect-complex-pattern-isomorphism rule"
  (id)
  (setf .id (ensure-canonical-complex-graph id nil))
  =>
  (setf .location-class (complex-graph-location-class .id #'monomer-symbol-p)))


;;;
;;; LABELS allowed in a COMPLEX-GRAPH
;;;

;;;
;;; MONOMER-LABEL
(defun monomer-label-p (x) (symbolp x))

;;;
;;; SITE-LABEL
(declaim (inline site-label-p site-label-sindex site-label-monomer site-label-value))
(defun make-site-label (monomer sindex &optional (value nil value-p))
  (if value-p (list sindex monomer value)
    (list sindex monomer)))
(defun site-label-p (x) 
  "true if x = (n d), where n is a number, d a monomer symbol"
  (and (consp x) (integerp (first x))))
(defun site-label-sindex (x) (first x))
(defun site-label-monomer (x) (second x))
(defun site-label-value (x) (third x))
(defun (setf site-label-value) (value x) (setf (third x) value))
(defun site-label-has-value-p (x) (> (length x) 2))

(defun complex-graph-label-to-site-labels (glabel)
  "Given a GLABEL, e.g., (0 KSR), returns the labels of that site."
  (site-info-tags (monomer-site-info (second glabel) (first glabel))))

;;;
;;; LOGICAL-LABEL:
;;; 
(defun ensure-args (args) (if (listp args) args (if (eq args '?) nil (list args))))
(defun logical-label-p (x)
  (and (consp x) (member (first x) '(and or not))))
(defun make-and-label (args) (list* 'and (ensure-args args)))
(defun make-or-label (args) (list* 'or (ensure-args args)))
(defun make-not-label (args) (list* 'not (ensure-args args)))
(defun logical-label-args (x) (rest x))

;;;
;;; WILDCARD-LABEL - for wildcard monomers
;;;
(defun make-wildcard-label (monomer 
                            sindex
                            &optional site-test (value-test nil value-test-p))
  (let ((tags-test    (normalize-test site-test 'and)))
    (if value-test-p
        (list sindex monomer tags-test value-test)
      (list sindex monomer tags-test))))

(declaim (inline wildcard-label-monomer wildcard-label-index
                 wildcard-label-site wildcard-label-state wildcard-label-p))
(defun wildcard-label-p (x) (and (consp x) (eq '? (second x))))
(defun wildcard-label-monomer (x) (first x))
(defun wildcard-label-index (x) (second x))
(defun wildcard-label-state (x) (fourth x))
(defun wildcard-label-tags (x) (third x))
(defun wildcard-state-label-p (x) (= (length x) 4))
(defun wildcard-monomer-symbol-p (x) (eq x '*))
(defun wildcard-binding-p (x) (eq x '*))
(defun wildcard-monomer-reference-p (x) 
  (or (wildcard-monomer-symbol-p x)
      (and (fld-form-p x)
           (wildcard-monomer-symbol-p (fld-form-object x)))))

;;;
;;; GRAPH GENERATION FUNCTIONS:
;;;

;;;
;;; MAKE-COMPLEX-GRAPH - the main function for generating graphs, 
;;;                      returns a complex graph, given a description
;;;
(defun make-complex-graph (&optional descr force-pattern)
  "Given a complex description of the form ((monomer1 b1 b2 ...) (monomer2 b3 b4...) ...),
   returns a canonical complex graph, and returns the location class of the graph"
  (multiple-value-bind (site-labels binding-table patternp)
      (parse-complex-description descr force-pattern)
    (values (build-complex-graph site-labels binding-table)
            patternp)))

(defun containing-location-class-for-monomers (monomers)
  (containing-location-class 
   (mapcar (lambda (d) (eval d).location-class) monomers)))

(defun unreference-graph-vertex-label (x)
  "Removes the field label from a graph label - 
   e.g., KSR.A -> KSR and (0 KSR.A) -> (0 KSR)"
  (cond
   ((fld-form-p x) (fld-form-object x))
   ((consp x)      (list* (first x) (fld-form-object (second x))
                          (cddr x)))
   (t              (error "Unexpected label ~S - expecting a FLD-FORM" x))))

(defun unreference-graph-labels (g)
  (map-into (gtools:graph-labels g)
            #'unreference-graph-vertex-label)
  g)
;;;
;;; BUILD-COMPLEX-GRAPH: internal work-horse which builds the actual GRAPH structure, 
;;;                      from data produced by the parser
;;;
(defun build-complex-graph (site-labels binding-table)
  "Takes a list of site-labels, value-labels and a table describing how they should be connected, and returns a non-canonical complex-graph"
  (loop for var being the hash-key of binding-table
        for bindings being the hash-value of binding-table

        if (bond-label-p var) ;; bond vars:
        nconc (case (length bindings)
                  (0 (error "Bug - var in bindings ht, but no bindings"))
                  (1 (if (plusp var) ; plusp vars are provided by the user ; minusp are autogenerated.
                         (error "Only one side of bond specified for ~S" var)
                       (list (list (first bindings) (first bindings)))))
                  ;; normally - 
                  (2 (list bindings))
                  ;; multiple bonds - ensure every site in the list 
                  ;; is connected to every other site in the list
                  (otherwise
                   ;; in future, maybe do a check here to ensure that all
                   ;; connector sites can support this many bonds
                   (maplist #'identity bindings))) into especs
        finally return 
        (gtools:make-labelled-graph (nconc especs (gethash '|monomer-edges| binding-table))
                                    site-labels
                                    nil
                                    '%make-complex-graph)))

(defvar *var-counter*
  "A counter for automatically generated bond vars") 
(defun make-bond-label ()
  (decf *var-counter*))
;;;
;;; PARSER: converts a list  the monomer & bindings to 
;;;              connector-labels - list of labels naming the connector sites
;;;              value-labels - list of labels for the sites which hold values
;;;              a binding-table - hash-table associating patterns vars with the
;;;                             verticies that they connect
;;;
(defun parse-complex-description (descr patternp)
  "Given a complex description of the form ((monomer1 b1 b2 ...) (monomer2 b3 b4...) ...),
   returns a non-canonical complex graph"
  (loop with *var-counter* = 0
        with binding-table = (make-hash-table :test #'equal)
        with pattern-detected-p = nil
        for component in descr
        for monomer = (first component)
        for bindings = (rest component)
        for offset = 0 then (+ offset (length site-labels))
        for (site-labels is-pattern-p) 
            = (multiple-value-list 
               (parse-monomer-description 
                monomer bindings binding-table offset patternp))
        do (if is-pattern-p (setf pattern-detected-p t))
        append site-labels into all-site-labels
        finally return (values
                        all-site-labels 
                        binding-table
                        pattern-detected-p)))

;;;
;;; PARSE-MONOMER-DESCRIPTION - figures out whether a pattern or concrete monomer is being described
;;;
(defun check-monomer-reference-fld-form-p (x bindings)  
  "Ensures X is a simple field form: A.B"
  (unless (and (fld-form-p x) (null (fld-form-args x)))
    (pattern-error 
     x bindings
     "Expecting a field form referencing a monomer of the form MONOMER.REF ~
            in [~S~{ ~^~S~}]" x bindings))
  (let ((field (fld-form-field x)))
    (when (bond-label-p field)
      (pattern-error
       x bindings "Invalid reference ~A integers are not allowed"
       field))))
       

(defun parse-monomer-description (head 
                                 bindings
                                 binding-table 
                                 offset
                                 patternp)
  "Given a description of the form (monomer b1 b2 b3...)
   RETURNS: SITE-LABELS, BONDS, OFFSET, PATTERNP
   Where SITE- and VALUE-LABELS are labels for the graph,
         BINDING-TABLE is a hash-table mapping binding vars to lists pairing sites offsets
                     with either other site offsets or values,
                     or to monomer indexes"
  (let ((monomer (cond
                 (patternp (check-monomer-reference-fld-form-p head bindings)
                           (fld-form-object head))
                 ((monomer-p head) head.name)
                 ((symbolp head)  head)
                 (t        (pattern-error head bindings "reference labelling (e.g., [MONOMER.LABEL ...]) only allowed in reaction definitions.")))))
    (cond
     ;; wildcard monomer
     ((wildcard-monomer-symbol-p monomer)
      (parse-wildcard-monomer-description 
       head
       bindings
       binding-table
       offset))

     ;; not a wildcard monomer
     ((symbolp monomer)
      (parse-named-monomer-description 
       monomer head bindings binding-table offset patternp))

     (t (error "Unrecognized monomer: ~S" monomer)))))


;;;
;;; RECORDING BONDS:
;;;
;;;; (defun record-monomer-site (mindex sindex binding-table)
;;;;   "Records bonds between the monomer indicies and the sites"
;;;;   (push sindex (gethash (coerce mindex 'float) binding-table)))

(defun record-monomer (mindex nsites binding-table)
  "Records the edges of the monomer in binding-table"
  (push (make-number-list mindex (+ mindex nsites))
        (gethash '|monomer-edges| binding-table)))

(defun make-number-list (start end &optional (increment 1))
  (loop for i from start to end by increment
        collect i))

(defun gen-bond-label (binding-table &optional (num -1))
  (if (gethash num binding-table) (gen-bond-label binding-table (1- num))
    num))

(defun record-bond-binding (sindex binding binding-table)
  "Records that site index is bound by variable indicated in BINDING.
  Returns nil on failure."
  (cond
   ((wildcard-bond-label-p binding) t) ; * means no bonds - used for patterns

   ((unbonded-label-p binding)
    (setf (gethash (gen-bond-label binding-table) 
                   binding-table)
          (list sindex sindex)))

   ((bond-label-p binding)
    (push sindex (gethash binding binding-table)))))

(defvar *default-site-binding* 'non-pattern-default-site-binding
  "A function which calculates the default binding to use for a site")

(defun non-pattern-default-site-binding (monomer site-num binding) 
  (or binding
      (let* ((site (svref (monomer-sites monomer) site-num)))
        (etypecase site
          (bond-site-info '_)
          (state-site-info (state-site-info-default site))))))
 
(defun default-site-binding (monomer site-num &optional binding) 
  (funcall *default-site-binding* monomer site-num binding))

(defun square-bracket-fld-form-p (x)
  (and (fld-form-p x)
       (eq (fld-form-field x) '#.(fld-form-field '().[]))))
;;;
;;; PARSE-NAMED-MONOMER-DESCRIPTION
;;;
(defun parse-named-monomer-description (monomer-object
                                         monomer
                                         bindings
                                         binding-table
                                         offset
                                         patternp)
  "When monomer is a symbol naming a defined monomer, and bindings are the 
   RETURNS: SITE-LABELS, PATTERNP
   Where site- and value-labels are labels for the graph,
         bonds is a hash-table mapping binding vars to lists pairing sites offsets
                     with either other site offsets or values
         PATTERNP is always NIL."
  (let ((pattern-detected-p nil)
        (sites              (map 'list #'identity (monomer-sites monomer-object))))
    (labels
        ((site-error (binding si msg &rest args)
           (b-error "Invalid binding ~S for site ~S in monomer ~S ~
                                     - ~?"
                    binding (site-info-tags si) monomer
                    msg args))
         (site-value-from-binding (binding si)
           (cond 
            ((eq '%%default binding)   (state-site-info-default si))
            ((wildcard-binding-p binding)  '*)
            ((typep binding (state-site-info-type si))  binding)
            ((and (consp binding) 
                  (every (lambda (x) (typep x (state-site-info-type si)))
                         binding))
             (unless patternp (pattern-error monomer bindings "patterns not allowed here."))
             binding)
            (t (site-error binding si
                           "expecting type ~S" (state-site-info-type si)))))
         (record-named-binding (binding i)
           (let* ((name   (keywordify (fld-form-object binding)))
                  (sinfo  (find name sites :key #'site-info-name)))
             (prog1
                 (etypecase sinfo
                   (bond-site-info (record-binding2 sinfo (fld-form-field binding) i))
                   (state-site-info (record-binding2 sinfo (fld-form-field binding) i))
                   (null (b-error "Unexpected site ~A, name is not one of ~S"
                                  binding (mapcar #'site-info-name sites))))
               (setf sites (delete name sites :key #'site-info-name)))))
         (record-binding (binding i) ; returns a site label on success
           (cond 
            ;; named binding: X.n or X.[...]
            ((fld-form-p binding)
             (record-named-binding binding i))

            ;; ordered binding:
            (t (record-binding2 (pop sites) binding i))))
         (record-binding2 (si binding i)
           (when (eq binding '%%default)
             (setf binding (default-site-binding monomer (site-info-index si))))
           (when (wildcard-binding-p binding) (setf pattern-detected-p t))
           (etypecase si
             (bond-site-info
              (unless (record-bond-binding i binding binding-table)
                (site-error binding si
                            "expecting _, * or a positive integer representing a bond label"))
              (list (site-info-index si) monomer))
             (state-site-info
              (list (site-info-index si) monomer
                    (site-value-from-binding binding si))))))

      (record-monomer offset (length sites) binding-table)
      (loop ;; connect the monomer node to the site nodes:
            for i from (1+ offset) to (+ offset (length sites))
            for rest-bindings = (or bindings '(%%default)) 
                           then (or (rest rest-bindings) '(%%default))
            for binding = (first rest-bindings)   
            for site-label = (record-binding  binding i)
            when site-label 
            collect site-label into site-labels
            finally return (values (list* monomer site-labels)
                                   pattern-detected-p)))))

(defun unkeywordify-fld-form-field (f)
  (let ((field (fld-form-field f)))
    (if (keywordp field) (intern (symbol-name field))
      field)))


;;;
;;; PARSE-WILDCARD-MONOMER-DESCRIPTION for [* ...] monomers
;;;
(defun parse-wildcard-monomer-description (monomer bindings binding-table offset)
  (flet ((make-label (i &optional site-test (value-test nil value-test-p))
             (let ((sindex      (- i offset 1))
                   (value-test  (normalize-test value-test 'or)))
               (apply #'make-wildcard-label 
                      monomer sindex site-test
                      (if value-test-p (list value-test))))))
                      
    (record-monomer offset (length bindings) binding-table)
    (loop for binding in bindings
          for i = (1+ offset) then (1+ i)
          if (fld-form-p binding)
            ;; [* TAG-TEST.[V1 V2...]] --- a value binding with label specification
            if (square-bracket-fld-form-p binding)
            collect (make-label i 
                                (fld-form-object binding)
                                (fld-form-args binding))
               into site-labels

            ;; [* TAG-TEST.1] -- bond binding
            else do (when (fld-form-args binding)
                      (error "Use ~S.[value] to specify a site with a value; ~
                              Use ~0@*~S.var to specify a bond; ~
                              Use ~0@*~S._ to specify an unconnected site."
                             (fld-form-object binding)))
                    (record-bond-binding i (fld-form-field binding)
                                         binding-table)
                 and collect (make-label i (fld-form-object binding))
                     into site-labels
  
         ;; [* 1] - a wildcard pattern with a bound site, no labels specified
         else if (user-bond-label-p binding)
              do (record-bond-binding i binding binding-table)
              and collect (make-label i '*) into site-labels
  
         ;; [* label-test] - a wildcard pattern with a label-test only (cnxns unspecified)
         else collect (make-label i binding) into site-labels
              
         finally return (values (list* monomer site-labels)
                              (1+ i)))))

(defun normalize-test (x &optional (logical-op 'and))
  (labels ((keywordify-logical-test (x)
             (list* (first x) 
                    (mapcar (lambda (elt)
                              (if (consp elt)
                                  (keywordify-logical-test elt)
                                (keywordify elt)))
                            (rest x)))))
    (cond
     ((wildcard-binding-p x) x)
     ((consp x)
      (keywordify-logical-test 
       (if (logical-label-p (first x))
           x
         (list* logical-op x))))
     (t (keywordify x)))))


;;;
;;; Variables accepted by the complex constructor syntax:
;;;

(defun unbonded-label-p (x) 
  (member x '(_ :_)))

(defun wildcard-bond-label-p (x)
  (member x '(* :*)))

(defun bond-label-p (x)
  "Objects used to denote a bond in a complex"
  (integerp x))

(defun user-bond-label-p (x)
  "Objects allowed to be used by the user to indicate a bond in a complex"
  (and (integerp x) (not (minusp x))))
  
;;;
;;;
;;; SQUARE BRACKET MACRO-READER
;;;

(defun monomer-form-p (o)
  (and (object-form-p o) 
       (monomer-symbol-p (object-form-object o))))

(defun complex-form-p (o)
  (and (object-form-p o)
       (monomer-form-p (object-form-object o))))

(defun monomer-symbol-ref-p (o)
  "MONOMER-SYMBOL or MONOMER-SYMBOL.REFVAR"
  (if (fld-form-p o) (monomer-symbol-p (fld-form-object o))
    (monomer-symbol-p o)))

(defun monomer-form-ref-p (o)
  (and (object-form-p o)
       (monomer-symbol-ref-p (object-form-object o))))

(defvar *reference-labels* nil)
(defun ensure-monomer-is-reference-labelled (dname)
  (cond
   ((fld-form-p dname) 
    (push (fld-form-field dname) (gethash nil *reference-labels*))
    dname)
   (t
    `.dname.,(make-reference-label dname))))

(defun make-az-string (n)
  (if (< n 26)
      (string (code-char (+ #.(char-code #\A) n)))
    (concatenate 'string 
                 (make-az-string (1- (truncate (/ n 26))))
                 (make-az-string (rem n 26)))))
    
(defun make-reference-label (dname)
  (labels ((gen-ref-label (&optional (n 0))
             (let ((newreflab (intern (make-az-string n) :keyword)))
               (cond
                ((find newreflab (gethash nil
                                          *reference-labels*))
                 (gen-ref-label (1+ n)))
                (t
                 (push newreflab (gethash nil *reference-labels*))
                 newreflab)))))
    (let ((id (cons dname (incf (gethash dname *reference-labels* 0)))))
      (or (gethash id *reference-labels*)
          (setf (gethash id *reference-labels*)
                (gen-ref-label))))))

(defmacro in-complex-pattern-mode (&body body)
  `(let ((*reference-labels* (make-hash-table :test #'equal)))
     ,@body))

(defun reset-reference-labels ()
  "Used to reset the counters on automatically determined reference labels"
  (loop for k being the hash-key in *reference-labels*
        when (monomer-symbol-p k)
        do (remhash k *reference-labels*)))

(defun complex-pattern-from-reference-pattern (rp)
  [complex-pattern (complex-graph-from-reference-graph rp)])

(defun complex-graph-from-reference-graph (rg)
  (let ((copy (gtools:copy-graph rg)))
    (unreference-graph-labels copy)
    (gtools:graph-delete-verticies-if
     copy
     (lambda (i)
       (selector-graph-ignorable-vertex-p rg i)))))
                                   
(defun selector-graph-ignorable-vertex-p (rg i)
  (let ((label (gtools:graph-vertex-label rg i)))
  (cond
   ((site-label-has-value-p label) 
    (eq label '*))
   (t (= (length (gtools:graph-vertex-outputs rg i)) 1)))))

;;;
;;; COMPLEX-EXPANDER: implements [] syntax
;;;
(add-object-expander 'complex-expander 0)
(defun complex-expander (args environment)
  (declare (ignorable environment))
  (let ((first (first args)))
    (cond
     ((monomer-symbol-ref-p first)
       `(make-complex '(,args)))
     ((monomer-form-ref-p first)
       `(make-complex ',(mapcar #'object-form-body args))))))

(defun make-complex (cdescr)  
  "Given a list of the form ((D1 1a 1b 1c) (D2 2a 2b 2c)...) where Dn are domain symbols and nx are bindings, constructs a complex"
  (cond
   (*reference-labels*    [reference-pattern 
                           (make-complex-graph
                            (mapcar
                             (lambda (ddescr) 
                               (list* (ensure-monomer-is-reference-labelled (first ddescr))
                                      (rest ddescr)))
                             cdescr)
                            t)])
   (t (if (complex-pattern-description-p cdescr)
          [complex-pattern cdescr]
        [complex-species-type cdescr]))))

(defun complex-pattern-description-p (cdescr)
  (some #'pattern-monomer-description-p cdescr))

(defun pattern-monomer-description-p (x)
  (and (consp x)
       (let ((head (first x)))
         (or (fld-form-p head)
             (some (lambda (x) (eq x '*)) x)))))


(defun firsthash (keys hash-table &optional default)
  "Returns the hash-value of a key in keys, otherwise default"
  (or (loop for k in keys
            for v = (gethash k hash-table #1='#:nohash)
            unless (eq v #1#)
            return v)
      default))



(defun complex-graph-description (graph)
  (let* ((mvertexes (complex-graph-monomer-vertexes graph))
         (cnxn-num  0)
         (var-table (make-hash-table :test #'equal))
         (labels    (gtools:graph-labels graph)))
    (labels ((add-bond (cnxns)
               (or (firsthash cnxns var-table)
                   (loop with new-var = (incf cnxn-num)
                         for c in cnxns
                         do (setf (gethash c var-table) new-var)
                         finally return new-var)))
             (calculate-bond-label (mindex sindex)               
               (let ((cnxns (complex-graph-site-bonds graph mindex sindex)))
                 (cond
                  ((null cnxns) '*)
                  ((equal cnxns `((,mindex ,sindex)))  ; connected to self = empty
                   '_)
                  (t  
                   (add-bond (list* (list mindex sindex) cnxns))))))
             (simplify-tags-test (x)
               (if (and (consp x) 
                        (eq (first x) 'and))
                   (rest x)
                 x))
             (simplify-state-test (x)
               (cond
                ((equal x '(or :*)) '(*))
                ((eq (first x) 'or) (rest x))
                (t x))))
                 
      (loop 
       for mindex = 0 then (1+ mindex)
       for mvertex in mvertexes
       for monomer = (elt labels mvertex)
       if (wildcard-monomer-reference-p monomer)
       collect 
       (list* monomer
              (loop for svertex in (complex-graph-monomer-site-vertexes graph mindex)
                    for site-label = (gtools:graph-vertex-label graph svertex)
                    for sindex = 0 then (1+ sindex)
                    for test-label = (simplify-tags-test (wildcard-label-tags site-label))
                    collect 
                    (cond 
                     ;; state sites:
                     ((wildcard-state-label-p site-label)
                      `,.test-label.[,@(simplify-state-test (wildcard-label-state site-label))])

                     ;; connector sites:
                     (t              
                      (let* ((blabel (keywordify (calculate-bond-label mindex sindex))))
                      `,.test-label.,blabel)))))

       else
       collect
       (list* monomer
              (loop with sinfos = (monomer-sites monomer)
                    for sindex from 0 below (length sinfos)
                    for sinfo = (svref sinfos sindex)
                    collect (cond
                             ;; state sites
                             ((state-site-info-p sinfo)
                              (site-label-value
                               (gtools:graph-vertex-label
                                graph 
                                (complex-graph-mindex-sindex-to-vertex graph mindex sindex))))
                             
                             ;; connector sites
                             ((bond-site-info-p sinfo)
                              (calculate-bond-label mindex sindex)))))))))

(defun complex-graph-constructor-form (graph)
  (let ((descr (complex-graph-description graph)))
    (case (length descr)
      (1 `[,@descr])
      (t `[,@(mapcar (lambda (mdescr)
                      `[,@mdescr])
                    descr)]))))

(defun complex-graph-site-bonds (g mindex sindex)
  "For 0-based monomer MINDEX and 0-based site SINDEX, returns a list of 
   monomer-site index cons pairs"
  (let* ((mvertex   (elt (complex-graph-monomer-vertexes g) mindex))
         (svertex   (elt (complex-graph-monomer-site-vertexes g mindex) sindex))
         (neighbors (delete mvertex (gtools:graph-vertex-outputs g svertex))))
    (mapcar (lambda (i)
              (multiple-value-list (complex-graph-mindex-sindex-from-vertex g i)))
            neighbors)))
    
(defun complex-graph-mindex-sindex-from-vertex (g i)
  (let* (; (labels   (gtools:graph-labels g))
         ; (nlabel   (elt labels i))
         (mvertexes (complex-graph-monomer-vertexes g))
         (mvertex   (first (intersection mvertexes
                                         (gtools:graph-vertex-outputs g i)))))
    (unless mvertex
      (error "Invalid input to ~S, ~S is not a site vertex"
             'complex-graph-resolve-site-index i))
    (let ((mindex    (position mvertex mvertexes)))
      (values mindex
              (position i (complex-graph-monomer-site-vertexes g mindex))))))
        

(defun complex-graph-monomer-vertexes (g)
  "Returns a list of vertex indexes representing monomers"
  (loop for i from 0 below (gtools:graph-vertex-count g)
        for lab = (gtools:graph-vertex-label g i)
        when (or (monomer-symbol-p lab)
                 (and (fld-form-p lab) (monomer-symbol-p (fld-form-object lab))))
        collect i))

(defun complex-graph-mindex-to-vertex (g mindex)
  (let ((dverts (complex-graph-monomer-vertexes g)))
    (cond
     ((>= mindex #1=(length dverts))
      (error "Monomer index out of range (~S).  ~S contains only ~S monomers."
             mindex g #1#))
     (t (nth mindex dverts)))))

(defun complex-graph-mindex-sindex-to-vertex (g mindex sindex)
  "Resolves a 0-based monomer/site specification to a vertex in the graph."
  (let ((sverts (complex-graph-monomer-site-vertexes g mindex)))
    (cond
     ((>= sindex #1=(length sverts))
      (error "Site index out of range (~S) for monomer ~S.  Only ~S sites for this monomer in ~S."
             sindex mindex #1# g))
     (t (nth sindex sverts)))))

(defun complex-graph-monomer-site-vertexes (g m)
  "Where g is a labelled graph and m is the 0-based monomer index.
Returns the vertexes representing sites in the correct order."
  (sort (gtools:graph-vertex-outputs 
         g
         (elt (complex-graph-monomer-vertexes g) m))
        (gtools:graph-type-label-less-predicate 'complex-graph)
        :key (lambda (i) (site-label-sindex (gtools:graph-vertex-label g i)))))


(defun pattern-error (monomer bindings &optional msg &rest args)
  (b-error "Invalid monomer [~S~{ ~S~}]~@[: ~?~]" monomer bindings msg args))


(defun complex-form-to-complex-description (o)
  (assert (object-form-p o))
  (let ((body (object-form-body o)))
    (cond
     ((object-form-p (first body)) (mapcar #'object-form-body body))
     (t                            (list body)))))
      

(defun complex-pattern-expander (form &optional objvar queryp)
  (when (or (monomer-form-p form)
            (complex-form-p form))
    (let ((cdescr (complex-form-to-complex-description form)))
      (cond
       ((and queryp
             (complex-pattern-description-p cdescr))
        (let ((pattern (eval form)))
          (values `([complex-pattern-match ,pattern ,objvar]) t)))
       (t      
        (let ((cst-graph (gentemp "?"))
              (testgraph (make-complex-graph cdescr)))
          (values `((,objvar [complex-species-type ,cst-graph])
                    (:test (gtools:find-subgraph-isomorphisms
                            ,testgraph
                            ,cst-graph nil))))))))))

(add-pattern-expander 'complex-pattern-expander)


;;;; (defun print-complex-graph (graph &optional 
;;;;                                   (stream *standard-output*)
;;;;                                   (prefix "[") 
;;;;                                   (suffix "]"))
;;;;   (let* ((mvertexes (complex-graph-monomer-vertexes graph))
;;;;         (multi     (> (length mvertexes) 1))
;;;;         (cnxn-num  0)
;;;;         (var-table (make-hash-table :test #'equal)))
;;;;     (flet ((add-bond (cnxns)
;;;;              (or (firsthash cnxns var-table)
;;;;                  (loop with new-var = (incf cnxn-num)
;;;;                        for c in cnxns
;;;;                        do (setf (gethash c var-table) new-var)
;;;;                        finally return new-var))))
;;;;       (pprint-logical-block (stream mvertexes
;;;;                                     :prefix (if multi prefix "")
;;;;                                     :suffix (if multi suffix ""))
;;;;         (loop 
;;;;          with labels = (gtools:graph-labels graph)
;;;;          for mindex = 0 then (1+ mindex)
;;;;          for mvertex in mvertexes
;;;;          for monomer = (elt labels mvertex)
;;;;          for sinfos = (if (wildcard-monomer-symbol-p monomer) nil (monomer-sites monomer))
;;;;          do (pprint-logical-block (stream () :prefix prefix :suffix suffix)
;;;;               (prin1 monomer stream)
;;;;               (loop for sindex from 0 below (length sinfos)
;;;;                     do (princ #\space stream)
;;;;                     (cond
;;;;                      ((state-site-info-p (svref sinfos sindex))
;;;;                       (prin1 (site-label-value
;;;;                               (gtools:graph-vertex-label
;;;;                                graph 
;;;;                                (complex-graph-mindex-sindex-to-vertex graph mindex sindex)))
;;;;                              stream))

;;;;                      ;; connector sites
;;;;                      (t
;;;;                       (let ((cnxns (complex-graph-site-bonds graph mindex sindex)))
;;;;                         (cond
;;;;                          ((null cnxns) (princ #\* stream))
;;;;                          ((equal cnxns `((,mindex ,sindex)))  ; connected to self = empty
;;;;                           (princ #\_ stream))
;;;;                          (t  
;;;;                           (princ (add-bond (list* (list mindex sindex) cnxns))
;;;;                                  stream)))))))
;;;;               (pprint-newline :linear stream)))))))