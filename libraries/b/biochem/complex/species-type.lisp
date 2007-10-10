
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

;;; $Id: species-type.lisp,v 1.1 2007/10/10 15:14:16 amallavarapu Exp $
;;; $Name:  $

;;; File: complex-speciestype.lisp
;;; Description: defines the complex-species-type structure
;;;              relies on the graph-tools library

;;;
;;; examples:
;;;    a species: [[ksr !1 _ _] [mapk !1]]
;;;    a reaction: {[ksr _] + [mapk _] -> [[ksr !1][mapk !1]]}
;;;            ==  {[ksr _1 * *] + [mapk _1 * *] -> [[ksr !1 * *][mapk !1 * *]]

;;;
;;;    [[d1-s-!1 *][d2-s-!1 *]] [d3...] -> [[d1 _]] + [d2-s!1-s!1-d3] 
;;;
;;;                                 -> might actually be [[d1 _ !2] 
;;;

;;;
;;; SITE - describes a site - 
;;; DOMAIN - a description of a fragment of a graph
;;; COMPLEX - a species-type which 
;;;
(in-package :b-user)

(include b/biochem/species :expose)

(port:define-dspec-class defsite () "")
(port:define-dspec-class defdomain () "")

;;;
;;; DOMAIN concept - main role is to hold sites
;;;
(defcon domain ()
  (&optional (name := *name*)
   &property (location-class := compartment)))

(defprop domain.sites ()
  (map 'vector (lambda (sspec)
                 (apply #'make-site-info (if (listp sspec) sspec (list sspec))))
       value))

(defield domain.site (x)
  (etypecase x
    (integer (svref .sites x))
    (symbol  (find x .sites :key #'site-info-symbol))))

(defgeneric domain-sites (d)
  (:method ((d symbol)) (|DOMAIN.SITES| (eval d)))
  (:method ((d domain)) (|DOMAIN.SITES| d))
  (:method ((d cons)) (domain-sites (fld-form-object d))))

(defun domain-site-info (d i)
  (svref (domain-sites d) i))

(defmethod documentation ((d domain) (type (eql :ctor-help)))
  (labels ((simplify-label (si) 
             (let ((labs (site-info-labels si)))
               (if (= (length labs) 1) (First labs) labs)))
           (value-site-doc (si)
             (list (simplify-label si)
                   (value-site-info-type si))))
    (format nil "COMPLEX CTOR: [~S~{ ~S~}]~@[ / CONNECTORS: ~{~S~^, ~}~]~@[ / VALUES: ~{~{~S.[~S]~}~^, ~}~]~@[ -- ~A~]"
            d.name
            (map 'list #'simplify-label d.sites)
            (map 'list #'site-info-symbol (remove-if-not #'connector-site-info-p d.sites))
            (map 'list #'value-site-doc (remove-if-not #'value-site-info-p d.sites))
            d.documentation)))

(port:define-dspec-form-parser defdomain (name)
  (if (consp name) `(defdomain ,(first name))
    `(defdomain ,name)))
(defmacro defdomain (name-lclass &body def)
  (mutils:let+ (((symbol lclass) (etypecase name-lclass
                                   (symbol (values name-lclass 'compartment))
                                   (cons   (values (first name-lclass) (second name-lclass)))))
                ((doc sites) (if (stringp #1=(first (last def))) (values #1# (butlast def))
                               (values nil def))))
    `(port:dspec (defdomain ,symbol)
       (define ,symbol [[domain] 
                        :location-class ,lclass
                        :sites ',sites
                       ,@(if doc `(:documentation ,doc))])
       ',symbol)))

(defun domain-symbol-p (o)
  (and (symbolp o)
       (domain-p (ignore-errors (eval o)))))

(predefine (:global complex-species-type)
(defmethod documentation ((d domain) (type (eql :fields-help)))
  (documentation complex-species-type :fields-help)))

;;;
;;; SITE-INFO structure
;;;
(defstruct (site-info (:constructor nil)) labels)

(defun site-info-symbol (s) (first (site-info-labels s)))

(defstruct (connector-site-info (:include site-info))
  type sublocation)

(defstruct (value-site-info (:include site-info))
  type default)

(defun member-type-p (x) (and (consp x) (eq (first x) 'member)))

(defun make-site-info (labels &rest args &key value default connector sublocation)
  (declare (ignorable args))
  (let* ((labels    (if (listp labels) labels (list labels)))   
         (symbol    (first labels))
         (xlabels   (rest labels)) ; extra labels
         (defsite   (get symbol 'site-info)) ; existing defined site
         (connector (or connector (not (or value default))))
         (value     (or value (and (not connector) (not sublocation))))
         (default    (or default 
                        (and (member-type-p value)
                             (second value)))))
    (cond
     ;; based on an already defined site:
     (defsite
      (when (or value connector)
        (error "Site ~S is already defined." (site-info-symbol labels)))
      (let ((new (copy-site-info defsite)))
        (setf (site-info-labels new) (append (site-info-labels new)
                                             xlabels))
        new))

     ;; attempt to provide both connector and value definitions:
     ((and value connector)
      (error "Ambiguous site definition ~S - value site or a connector site?"))

     ;; when it's a not a connector, check default value is valid
     ((not (or connector
               (typep default value)))
      (error "Default value ~S is not of type ~S in site ~S"
             default value symbol))

    
     (value (make-value-site-info :labels labels :type value :default default))

     (t     (make-connector-site-info :labels labels
                                      :type connector
                                      :sublocation sublocation)))))

(defun define-site (symbol &rest keys &key value default connector)
    (setf (get symbol 'site-info) (apply #'make-site-info symbol keys)))

(defmacro defsite (symbol &rest keys &key values default allow disallow)
  `(port:dspec (defsite ,symbol)
     (remprop ',symbol 'site-info)
     (apply #'define-site ',symbol ',keys)
     ',symbol))




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
                  (number nil)
                  (cons   t)))
               (number   (etypecase y
                           (number (< x y))
                           (symbol t)
                           (cons   t)))
               (cons
                (etypecase y
                  (symbol nil)
                  (number nil)
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
             (let ((site-test (wildcard-label-site test))
                   (value-test (wildcard-label-value test)))
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
                     (equal (site-label-domain test) (site-label-domain glabel))
                     (value-test (site-label-value test) (site-label-value glabel))))

               (t (error "Unexpected test graph label:~S" test)))))
    #'complex-graph-label-test-predicate))

;;;
;;;  COMPLEX-SPECIES-TYPE:
;;;
;;;
(defcon complex-species-type (species-type)
  "SPECIES-TYPE which is described as a graph"
  (&optional (id 
              :documentation "A list specifying domains and their connectivity"))
  (etypecase id
    (complex-graph nil)
    (cons          (setf .id (gtools:canonical-graph 
                              (etypecase id
                                (complex-graph  id)
                                (cons           (make-complex-graph id nil))))))))

(defield complex-species-type.domains ()
  (remove-if-not #'symbolp (gtools:labelled-graph-labels .id)))

(defield complex-species-type.site-connection (dindex sindex)
  (complex-graph-site-connection .id dindex sindex))


;;;
;;; LABELS allowed in a COMPLEX-GRAPH
;;;

;;;
;;; DOMAIN-LABEL
(defun domain-label-p (x) (symbolp x))

;;;
;;; SITE-LABEL
(declaim (inline site-label-p site-label-sindex site-label-domain site-label-value))
(defun make-site-label (domain sindex &optional (value nil value-p))
  (if value-p (list sindex domain value)
    (list sindex domain)))
(defun site-label-p (x) 
  "true if x = (n d), where n is a number, d a domain symbol"
  (and (consp x) (integerp (first x))))
(defun site-label-sindex (x) (first x))
(defun site-label-domain (x) (second x))
(defun site-label-value (x) (third x))
(defun (setf site-label-value) (value x) (setf (third x) value))
(defun site-label-has-value-p (x) (> (length x) 2))

(defun complex-graph-label-to-site-labels (glabel)
  "Given a GLABEL, e.g., (0 KSR), returns the labels of that site."
  (site-info-labels (domain-site-info (second glabel) (first glabel))))

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
;;; WILDCARD-LABEL - for wildcard domains
;;;
(defun make-wildcard-label (domain 
                            sindex
                            &optional site-test (value-test nil value-test-p))
  (let ((nst    (normalize-test site-test 'and)))
    (if value-test-p
        (list sindex domain nst value-test)
      (list sindex domain nst))))

(declaim (inline wildcard-label-domain wildcard-label-index
                 wildcard-label-site wildcard-label-value wildcard-label-p))
(defun wildcard-label-p (x) (and (consp x) (eq '? (second x))))
(defun wildcard-label-domain (x) (first x))
(defun wildcard-label-index (x) (second x))
(defun wildcard-label-value (x) (third x))
(defun wildcard-label-site (x) (fourth x))


;;;
;;; GRAPH GENERATION FUNCTIONS:
;;;

;;;
;;; MAKE-COMPLEX-GRAPH - the main function for generating graphs, 
;;;                      returns a complex graph, given a description
;;;
(defun make-complex-graph (&optional descr (patternp t))
  "Given a complex description of the form ((domain1 b1 b2 ...) (domain2 b3 b4...) ...),
   returns a canonical complex graph, and returns the location class of the graph"
  (multiple-value-bind (site-labels binding-table) 
      (parse-complex-description descr patternp)
    (values (build-complex-graph site-labels binding-table))))

;;;;             (containing-location-class-for-domain-symbol-refs
;;;;              (if patternp (mapcar #'unreference-graph-vertex-label
;;;;                                   (remove-if-not #'domain-symbol-ref-p site-labels))
;;;;                (remove-if-not #'domain-symbol-p site-labels))))))

(defun containing-location-class-for-domains (domains)
  (containing-location-class 
   (mapcar (lambda (d) (eval d).location-class) domains)))

(defun unreference-graph-vertex-label (x)
  "Removes the field label from a graph label - 
   e.g., KSR.A -> KSR and (0 KSR.A) -> (0 KSR)"
  (cond
   ((fld-form-p x) (fld-form-object x))
   ((consp x)      (list* (first x) (fld-form-object (second x))
                          (cddr x)))
   (t              (error "Unexpected label ~S - expecting a FLD-FORM" x))))

(defun unreference-graph-labels (g)
  (map-into (gtools:labelled-graph-labels g)
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

        if (connection-var-p var) ;; connection vars:
        nconc (case (length bindings)
                  (0 (error "Bug - var in bindings ht, but no bindings"))
                  (1 (if (plusp var) ; plusp vars are provided by the user ; minusp are autogenerated.
                         (error "Only one side of connection specified for ~S" var)
                       (list (list (first bindings) (first bindings)))))
                  ;; normally - 
                  (2 (list bindings))
                  ;; multiple connections - ensure every site in the list 
                  ;; is connected to every other site in the list
                  (otherwise
                   ;; in future, maybe do a check here to ensure that all
                   ;; connector sites can support this many connections
                   (maplist #'identity bindings))) into especs
;;;;         else if (floatp var)
;;;;         collect (list* (rationalize var) bindings) into especs
;;;;         no need because it's an unlabelled graph now
;;;;         and nconc (mapcar (lambda (s) `(,s ,var))
;;;;                           bindings) into especs
        finally return 
        (gtools:make-labelled-graph (nconc especs (gethash '|domain-edges| binding-table))
                                    site-labels
                                    nil
                                    '%make-complex-graph)))

(defvar *var-counter*
  "A counter for automatically generated connection vars") 
(defun make-connection-var ()
  (decf *var-counter*))
;;;
;;; PARSER: converts a list  the domain & bindings to 
;;;              connector-labels - list of labels naming the connector sites
;;;              value-labels - list of labels for the sites which hold values
;;;              a binding-table - hash-table associating patterns vars with the
;;;                             verticies that they connect
;;;
(defun parse-complex-description (descr patternp)
  "Given a complex description of the form ((domain1 b1 b2 ...) (domain2 b3 b4...) ...),
   returns a non-canonical complex graph"
  (loop with *var-counter* = 0
        with binding-table = (make-hash-table :test #'equal)
        with pattern-detected-p = nil
        for component in descr
        for domain = (first component)
        for bindings = (rest component)
        for offset = 0 then (+ offset (length site-labels))
        for (site-labels is-pattern-p) 
            = (multiple-value-list 
               (parse-domain-description 
                domain bindings binding-table offset patternp))
        do (if is-pattern-p (setf pattern-detected-p t))
        append site-labels into all-site-labels
        finally return (values
                        all-site-labels 
                        binding-table
                        pattern-detected-p)))


;;;
;;; PARSE-DOMAIN-DESCRIPTION - figures out whether a pattern or concrete domain is being described
;;;
(defun check-domain-reference-fld-form-p (x bindings)  
  "Ensures X is a simple field form: A.B"
  (unless (and (fld-form-p x) (null (fld-form-args x)))
    (pattern-error 
     x bindings
     "Expecting a field form referencing a domain of the form DOMAIN.REF ~
            in [~S~{ ~^~S~}]" x bindings))
  (let ((field (fld-form-field x)))
    (when (connection-var-p field)
      (pattern-error
       x bindings "Invalid reference ~A integers are not allowed"
       field))))
       

(defun parse-domain-description (head 
                                 bindings
                                 binding-table 
                                 offset
                                 patternp)
  "Given a description of the form (domain b1 b2 b3...)
   RETURNS: SITE-LABELS, CONNECTIONS, OFFSET, PATTERNP
   Where SITE- and VALUE-LABELS are labels for the graph,
         BINDING-TABLE is a hash-table mapping binding vars to lists pairing sites offsets
                     with either other site offsets or values,
                     or to domain indexes"
  (let ((domain (cond
                 (patternp (check-domain-reference-fld-form-p head bindings)
                           (fld-form-object head))
                 ((domain-p head) head.name)
                 ((symbolp head)  head)
                 (t        (pattern-error head bindings "pattern not allowed here.")))))
    (cond
     ;; pattern domain
     ((eq domain '*)
      (parse-wildcard-domain-description 
       head
       bindings
       binding-table
       offset))

     ;; not a pattern domain
     ((symbolp domain)
      (parse-ordered-domain-description 
       domain head bindings binding-table offset patternp))

     (t (error "Unrecognized domain: ~S" domain)))))


;;;
;;; RECORDING CONNECTIONS:
;;;
;;;; (defun record-domain-site (dindex sindex binding-table)
;;;;   "Records connections between the domain indicies and the sites"
;;;;   (push sindex (gethash (coerce dindex 'float) binding-table)))

(defun record-domain (dindex nsites binding-table)
  "Records the edges of the domain in binding-table"
  (push (make-number-list dindex (+ dindex nsites))
        (gethash '|domain-edges| binding-table)))

(defun make-number-list (start end &optional (increment 1))
  (loop for i from start to end by increment
        collect i))

(defun gen-connection-var (binding-table &optional (num -1))
  (if (gethash num binding-table) (gen-connection-var binding-table (1- num))
    num))

(defun record-connection-binding (sindex binding binding-table)
  "Records that site index is bound by variable indicated in BINDING.
  Returns nil on failure."
  (cond
   ((eq binding '*) t) ; * means no connections - used for patterns

   ((unconnected-binding-p binding)
    (setf (gethash (gen-connection-var binding-table) 
                   binding-table)
          (list sindex sindex)))

   ((connection-var-p binding)
    (push sindex (gethash binding binding-table)))))

(defvar *default-site-binding* 'non-pattern-default-site-binding
  "A function which calculates the default binding to use for a site")

(defun non-pattern-default-site-binding (domain site-num binding) 
  (or binding
      (let* ((site (svref (domain-sites domain) site-num)))
        (etypecase site
          (connector-site-info '_)
          (value-site-info (value-site-info-default site))))))
 
(defun default-site-binding (domain site-num &optional binding) 
  (funcall *default-site-binding* domain site-num binding))

;;;
;;; PARSE-ORDERED-DOMAIN-DESCRIPTION
;;;
(defun parse-ordered-domain-description (domain-object
                                         domain
                                         bindings
                                         binding-table
                                         offset
                                         patternp)
  "When domain is a symbol naming a defined domain, and bindings are the 
   RETURNS: SITE-LABELS, PATTERNP
   Where site- and value-labels are labels for the graph,
         connections is a hash-table mapping binding vars to lists pairing sites offsets
                     with either other site offsets or values
         PATTERNP is always NIL."
  (let ((pattern-detected-p nil)
        (sites              (domain-sites domain-object)))
    (labels
        ((site-error (binding si msg &rest args)
           (b-error "Invalid binding ~S for site ~S in domain ~S ~
                                     - ~?"
                    binding (site-info-labels si) domain
                    msg args))
         (site-value-from-binding (binding si)
           (cond 
            ((eq :default binding)   (value-site-info-default si))
            ((eq '* binding)         '*)
            ((typep binding (value-site-info-type si))  binding)
            ((and (consp binding) 
                  (every (lambda (x) (typep x (value-site-info-type si)))
                         binding))
             (unless patternp (pattern-error domain bindings "patterns not allowed here."))
             binding)
            (t (site-error binding si
                           "expecting type ~S" (value-site-info-type si)))))
         (record-binding (si binding i) ; returns a site label on success
           (when (eq binding '*) 
             (setf pattern-detected-p t)
             (unless patternp (pattern-error domain bindings "patterns not allowed here.")))
           (let ((sindex (position si sites)))
             ;(record-domain-site offset i binding-table)
             (etypecase si
               (connector-site-info
                (unless (record-connection-binding i binding binding-table)
                  (site-error binding si
                              "expecting _ or a connection a positive integer"))
                (list sindex domain))
               (value-site-info
                (list sindex domain
                      (site-value-from-binding binding si)))))))
      (record-domain offset (length sites) binding-table)
      (loop ;; connect the domain node to the site nodes:
            with i = (1+ offset) 
            for si across sites
            for rest-bindings = bindings then (rest rest-bindings)
            for binding = (first rest-bindings)  
            for site-label = (record-binding 
                              si 
                              (default-site-binding
                               domain (- i offset 1)
                               binding)
                              i)
            when site-label 
            collect site-label into site-labels
            and do (incf i)
            finally return (values (list* domain site-labels)
                                   pattern-detected-p)))))

(defun unkeywordify-fld-form-field (f)
  (let ((field (fld-form-field f)))
    (if (keywordp field) (intern (symbol-name field))
      field)))


;;;
;;; PARSE-WILDCARD-DOMAIN-DESCRIPTION for [? ...] domains
;;;
(defun parse-wildcard-domain-description (domain bindings binding-table offset)
  (flet ((make-label (i &optional site-test value-test)
           ;(record-domain-site offset i binding-table)
           (let ((sindex (- i offset 1)))
             (make-wildcard-label domain sindex site-test value-test))))
    (record-domain offset (length bindings) binding-table)
    (loop for binding in bindings
          for i = (1+ offset) then (1+ i)
          if (fld-form-p binding)
            ;; [? L.[V1 V2...]] --- a value binding with label specification
            if (eq '#.(fld-form-field '().[]) (fld-form-field binding))
               collect (make-label i 
                                        (fld-form-object binding)
                                        (fld-form-args binding))
               into site-labels

            ;; [? site-test.1] -- connection binding
            else do (when (fld-form-args binding)
                      (error "Use ~S.[value] to specify a site with a value; ~
                              Use ~0@*~S.!var to specify a connection; ~
                              Use ~0@*~S._ to specify an unconnected site."
                             (fld-form-object binding)))
                    (record-connection-binding i (fld-form-field binding)
                                               binding-table)
                 and collect (make-label i (fld-form-object binding))
                     into site-labels
  
         ;; [? 1] - a wildcard pattern with a bound site, no labels specified
         else if (user-connection-var-p binding)
              do (record-connection-binding i binding binding-table)
              and collect (make-label i) into site-labels
  
         ;; [? label-test] - a wildcard pattern with a label-test only (cnxns unspecified)
         else collect (make-label i binding) into site-labels
              
         finally return (values (list* domain site-labels)
                              (1+ i)))))

(defun normalize-test (x &optional (logical-op 'and))
  (cond
   ((eq x '?) (list logical-op))
   ((and (not (logical-label-p x))
         (consp x))     (list* logical-op x))
   (t         x)))


;;;
;;; Variables accepted by the complex constructor syntax:
;;;

(defun unconnected-binding-p (x) 
  (and (symbolp x) (string= (symbol-name x) "_")))

(defun connection-var-p (x)
  "Objects used to denote a connection in a complex"
  (integerp x))

(defun user-connection-var-p (x)
  "Objects allowed to be used by the user to indicate a connection in a complex"
  (and (integerp x) (not (minusp x))))
  
;;;
;;;
;;; SQUARE BRACKET MACRO-READER
;;;

(defun domain-form-p (o)
  (and (object-form-p o) 
       (domain-symbol-p (object-form-object o))))

(defun complex-form-p (o)
  (and (object-form-p o)
       (domain-form-p (object-form-object o))))

(defun domain-symbol-ref-p (o)
  "DOMAIN-SYMBOL or DOMAIN-SYMBOL.REFVAR"
  (if (fld-form-p o) (domain-symbol-p (fld-form-object o))
    (domain-symbol-p o)))

(defun domain-form-ref-p (o)
  (and (object-form-p o)
       (domain-symbol-ref-p (object-form-object o))))

(defvar *reference-labels* nil)
(defun ensure-domain-is-reference-labelled (dname)
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

(defmacro in-reference-pattern-mode (&body body)
  `(let ((*reference-labels* (make-hash-table :test #'equal)))
     ,@body))

(defun reset-reference-labels ()
  "Used to reset the counters on automatically determined reference labels"
  (loop for k being the hash-key in *reference-labels*
        when (domain-symbol-p k)
        do (remhash k *reference-labels*)))

(defun selector-from-reference-pattern (rp)
  [selector-pattern (selector-graph-from-reference-graph rp)])

(defun selector-graph-from-reference-graph (rg)
  (let ((copy (gtools:copy-graph rg)))
    (unreference-graph-labels copy)
    (gtools:graph-delete-verticies-if
     copy
     (lambda (i)
       (selector-graph-ignorable-vertex-p rg i)))))
                                   
(defun selector-graph-ignorable-vertex-p (rg i)
  (let ((label (gtools:labelled-graph-vertex-label rg i)))
  (cond
   ((site-label-has-value-p label) 
    (eq label '*))
   (t (= (length (gtools:graph-vertex-outputs rg i)) 1)))))

(add-object-expander 'complex-expander 0)
(defun complex-expander (args environment)
  (declare (ignorable environment))
  (let ((first (first args)))
    (cond
     ((domain-symbol-ref-p first)
       `(complex-graph-ctor '(,args)))
     ((domain-form-ref-p first)
       `(complex-graph-ctor ',(mapcar #'object-form-body args))))))

(defun complex-graph-ctor (cdescr)  
  (if *reference-labels*
      [reference-pattern 
       (make-complex-graph
        (mapcar
         (lambda (ddescr) 
           (list* (ensure-domain-is-reference-labelled (first ddescr))
                  (rest ddescr)))
         cdescr)
        t)]
    [complex-species-type cdescr]))
  
(defmethod print-concept ((o complex-species-type) &optional stream)
  (let ((graph     o.id))
    (print-complex-graph graph stream "[" "]")))

(defun firsthash (keys hash-table &optional default)
  "Returns the hash-value of a key in keys, otherwise default"
  (or (loop for k in keys
            for v = (gethash k hash-table #1='#:nohash)
            unless (eq v #1#)
            return v)
      default))

(defun print-complex-graph (graph &optional 
                                  (stream *standard-output*)
                                  (prefix "[") 
                                  (suffix "]"))
  (let* ((dvertexes (complex-graph-domain-vertexes graph))
        (multi     (> (length dvertexes) 1))
        (cnxn-num  0)
        (var-table (make-hash-table :test #'equal)))
    (flet ((add-connection (cnxns)
             (or (firsthash cnxns var-table)
                 (loop with new-var = (incf cnxn-num)
                       for c in cnxns
                       do (setf (gethash c var-table) new-var)
                       finally return new-var))))
      (pprint-logical-block (stream dvertexes
                                    :prefix (if multi prefix "")
                                    :suffix (if multi suffix ""))
        (loop 
         with labels = (gtools:labelled-graph-labels graph)
         for dindex = 0 then (1+ dindex)
         for dvertex in dvertexes
         for domain = (elt labels dvertex)
         for sinfos = (domain-sites domain)
         do (pprint-logical-block (stream () :prefix prefix :suffix suffix)
              (prin1 domain stream)
              (loop for sindex from 0 below (length sinfos)
                    do (princ #\space stream)
                    (cond
                     ((value-site-info-p (svref sinfos sindex))
                      (prin1 (site-label-value
                              (gtools:labelled-graph-vertex-label
                               graph 
                               (complex-graph-dindex-sindex-to-vertex graph dindex sindex)))
                             stream))

                     ;; connector sites
                     (t
                      (let ((cnxns (complex-graph-site-connections graph dindex sindex)))
                        (cond
                         ((null cnxns) (princ #\* stream))
                         ((equal cnxns `((,dindex ,sindex)))  ; connected to self = empty
                          (princ #\_ stream))
                         (t  
                          (princ (add-connection (list* (list dindex sindex) cnxns))
                                 stream)))))))
              (pprint-newline :linear stream)))))))

(defun complex-graph-site-connections (g dindex sindex)
  "For 0-based domain DINDEX and 0-based site SINDEX, returns a list of 
   domain-site index cons pairs"
  (let* ((dvertex   (elt (complex-graph-domain-vertexes g) dindex))
         (svertex   (elt (complex-graph-domain-site-vertexes g dindex) sindex))
         (label     (gtools:labelled-graph-vertex-label g svertex))    
         (neighbors (delete dvertex (gtools:graph-vertex-outputs g svertex))))
    (mapcar (lambda (i)
              (multiple-value-list (complex-graph-dindex-sindex-from-vertex g i)))
            neighbors)))
    
(defun complex-graph-dindex-sindex-from-vertex (g i)
  (let* ((labels   (gtools:labelled-graph-labels g))
         (nlabel   (elt labels i))
         (dvertexes (complex-graph-domain-vertexes g))
         (dvertex   (first (intersection dvertexes
                                         (gtools:graph-vertex-outputs g i)))))
    (unless dvertex
      (error "Invalid input to ~S, ~S is not a site vertex"
             'complex-graph-resolve-site-index i))
    (let ((dindex    (position dvertex dvertexes)))
      (values dindex
              (position i (complex-graph-domain-site-vertexes g dindex))))))
        

(defun complex-graph-domain-vertexes (g)
  "Returns a list of vertex indexes representing domains"
  (loop for i from 0 below (gtools:graph-vertex-count g)
        for lab = (gtools:labelled-graph-vertex-label g i)
        when (or (domain-symbol-p lab)
                 (and (fld-form-p lab) (domain-symbol-p (fld-form-object lab))))
        collect i))

(defun complex-graph-dindex-to-vertex (g dindex)
  (let ((dverts (complex-graph-domain-vertexes g)))
    (cond
     ((>= dindex #1=(length dverts))
      (error "Domain index out of range (~S).  ~S contains only ~S domains."
             dindex g #1#))
     (t (nth dindex dverts)))))

(defun complex-graph-dindex-sindex-to-vertex (g dindex sindex)
  "Resolves a 0-based domain/site specification to a vertex in the graph."
  (let ((sverts (complex-graph-domain-site-vertexes g dindex)))
    (cond
     ((>= sindex #1=(length sverts))
      (error "Site index out of range (~S) for domain ~S.  Only ~S sites for this domain in ~S."
             sindex dindex #1# g))
     (t (nth sindex sverts)))))

(defun complex-graph-domain-site-vertexes (g d)
  "Where g is a labelled graph and d is the 0-based domain index.
Returns the vertexes representing sites in the correct order."
  (sort (gtools:graph-vertex-outputs 
         g
         (elt (complex-graph-domain-vertexes g) d))
        (gtools:graph-type-label-less-predicate 'complex-graph)
        :key (lambda (i) (site-label-sindex (gtools:labelled-graph-vertex-label g i)))))


(defun pattern-error (domain bindings &optional msg &rest args)
  (b-error "Invalid domain [~S~{ ~S~}]~@[: ~?~]" domain bindings msg args))


