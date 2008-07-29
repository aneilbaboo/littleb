
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

;;; $Id: reaction-type.lisp,v 1.24 2008/07/29 15:49:19 amallavarapu Exp $
;;; $Name:  $

;;; File: complex-reaction-type.lisp
;;; Description: defines the complex-species-type structure
;;;              relies on the graph-tools library

(in-package #I@library/biochem)

(include (b/math @folder/species-type @library/biochem/reaction-type))

(def-macro-operator ->> ((+ 1 (operator-precedence '+)) :xfy)
    (lhs rhs)
  (let ((lhsvar '#:lhs)
        (rhsvar '#:rhs))
  `(with-complex-reaction-type-parts ((,lhsvar ,lhs) (,rhsvar ,rhs))
     (if (localization-p ,lhsvar)
         [complex-reaction-type ,.lhsvar.entity ,rhsvar ,.lhsvar.location]
       [complex-reaction-type ,lhsvar ,rhsvar]))))

(defcon complex-reaction-type ()
  (lhs 
   rhs
   &optional
   (location-class := nil))
  (multiple-value-bind (lhs rhs lclass)
      (parse-reaction-type-arguments lhs rhs location-class '->> #'canonicalize-complex-pattern)
    (setf .lhs (apply #'s+ lhs)
          .rhs (apply #'s+ rhs)
          .location-class lclass)))

(defcon reversible-complex-reaction-type ()
  (lhs rhs &optional location-class
       &property
       (fwd :#= [complex-reaction-type .lhs .rhs .location-class])
       (rev :#= [complex-reaction-type .rhs .lhs .location-class])))
  

(def-macro-operator <<->> ((+ 1 (operator-precedence '+)) :xfy)
    (lhs rhs)
  (let ((lhsvar '#:lhs)
        (rhsvar '#:rhs)
        (lclass '#:lclass))
    `(with-complex-reaction-type-parts ((,lhsvar ,lhs) (,rhsvar ,rhs))
       (let+ (((,lhsvar ,lclass) (if (localization-p ,lhsvar)
                                     (values ,.lhsvar.entity ,.lhsvar.location)
                                   (values ,lhsvar nil))))
         [reversible-complex-reaction-type ,lhsvar ,rhsvar ,lclass]))))
;;;;
;;;; PRINTING:

(defmethod print-concept ((o complex-reaction-type) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression o stream t)))

(defmethod print-math-expression 
           ((o complex-reaction-type)
            &optional (stream *standard-output*) (outer-op t))
  (pprint-math-form `{,o.lhs ->> ,o.rhs} stream outer-op))

(defmethod print-concept ((o reversible-complex-reaction-type) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression o stream t)))

(defmethod print-math-expression 
           ((o reversible-complex-reaction-type)
            &optional (stream *standard-output*) (outer-op t))
  (pprint-math-form `{,o.lhs <<->> ,o.rhs} stream outer-op))

;;;;
;;;; REACTION-TYPE ARGUMENT PARSING, VALIDATION:
;;;; 
(defun canonicalize-complex-pattern (x)
  (etypecase x
    (monomer               (canonicalize-complex-pattern (eval `[,x.name])))
    (complex-species-type  [reference-pattern x.id])
    (reference-pattern     x)))

    
(defmacro with-complex-reaction-type-parts (((lhs lhs-form)
                                        (rhs rhs-form))
                                       &body body)
  `(in-complex-pattern-mode
     (let* ((,lhs {,lhs-form})
            (*default-site-binding* (lhs-default-site-binding-function 
                                     (graphs-in-expression ,lhs))))
       (reset-reference-labels) ; resets counters for automatically generated monomer reference labels
       (let ((,rhs {,rhs-form}))
         (declare (ignorable ,rhs))
         ,@body))))
     
(defun location-definition-p (x)
  (and (localization-p x)
       (location-class-p x.location)))

(deftype location-definition ()
  `(satisfies location-definition-p))

(defun graphs-in-expression (x)
  "Returns complex-graph objects plus the corresponding vars in the expression"
  (etypecase x
    ;; expr @ location-class
    ((cons location-definition)  (graphs-in-expression (first x).entity))
    ;; (a b c ...)
    (list           (loop for elt in x
                          collect elt into rxn-vars
                          collect (typecase elt
                                    (reference-pattern elt.id) ;; IN MAIN LOCATION
                                    (localization elt.entity.id) ;; IN SUBLOCATION
                                    (t (b-error "Invalid input to complex-reaction-type: ~S" elt)))
                          into graphs
                          finally (return (values graphs rxn-vars))))
    ;; {a + b + c...}
    (sum-expression (graphs-in-expression x.vars))

    ;; a
    (t              (graphs-in-expression (list x)))))
                           

(defun extract-canonical-graphs-from-expression (expr)
  (mutils:let+ (((graphs rxn-vars) (graphs-in-expression expr)))
    (loop for g in graphs
          for (cg r) = (multiple-value-list (gtools:canonical-graph (graph-remove-reference-labels g)))
          for rg = (gtools:reorder-graph g r)
          collect cg into dereferenced-graphs
          collect rg into reference-graphs
          finally (return (values reference-graphs dereferenced-graphs rxn-vars)))))


;;;
;;; Named bond - a pair (c1 c2) where c1 and c2 are vertex names
;;;                    (either a fld-form MONOMER.REF 
;;;                         site ref (SITE-INDEX MONOMER.REF &optional VALUE)
;;; 
(defun compute-bond-list (binding-table site-labels)
  "Returns a list of sites and a list of "
  (loop with sites = (apply #'vector site-labels)
        for k being the hash-keys of binding-table
        for v being the hash-values of binding-table
        ;; a bond
        if (bond-label-p k)
        collect (mapcar (lambda (i) (svref sites i)) v)))

(defun compute-monomers (site-labels)
  (remove-if-not (lambda (x) (and (fld-form-p x)
                                  (monomer-label-p (fld-form-object x))))
                 site-labels))

(defun named-vertex= (n1 n2)
  (cond
   ((and (consp n1) (consp n2))
    (or (equal n1 n2)
        (and (numberp (first n1))
             (eql (first n1) (first n2))
             (equal (second n1) (second n2)))))
   (t (eq n1 n2))))
      
(defun named-bond= (n1 n2)
  (or (every #'named-vertex= n1 n2)
      (every #'named-vertex= n1 (reverse n2))))


(defun binarize-bond (cnxn)
  (let ((from (first cnxn)))
    (append (mapcar (lambda (to) (list from to)) (rest cnxn))
            (if (> (length cnxn) 2)
                (binarize-bond (rest cnxn))))))

(defun binarize-bonds (cnxns) (mapcan #'binarize-bond cnxns))

(defun compute-bond-set-difference (x y)
  "Given two bond-lists, computes the difference between the two"
  (set-difference (binarize-bonds x) (binarize-bonds y)
                  :test #'named-vertex=))

(defun automatic-bond-label-p (x)
  (and (bond-label-p x)
       (null (symbol-package x))))

(defun fix-automatic-bond-labels (binding-table)
  "If we have made auto-generated bonds vars which have only one bond side,
   convert these to self-bond (_) bonds."
  (loop for v being the hash-keys of binding-table
        for cnxns being the hash-values of binding-table
        when (eq (length cnxns) 1)
        do (push (first cnxns) (gethash v binding-table))))

(defun named-vertex-to-graph-offset (nvert named-offsets)
  (cdr (assoc nvert named-offsets :test #'named-vertex=)))

(defun named-cnxn-to-graph-offsets (ncnxn named-offsets)
  (list (named-vertex-to-graph-offset (first ncnxn) named-offsets)
        (named-vertex-to-graph-offset (second ncnxn) named-offsets)))

(defun compute-vertex-set-difference (x y)
  (set-difference x y :test #'named-vertex=))

(defun compute-label-changes (lhs rhs)
  (loop for lvert across lhs
        for rvert = (if (site-label-p lvert)
                        (find lvert rhs :test #'named-vertex=))
        for lstate = (site-label-state lvert)
        for rstate = (site-label-state rvert)
        unless (or (null rvert)
                   (equal lstate rstate)
                   (eq rstate '*))
        collect (list (make-site-label (site-label-monomer lvert)
                                       (site-label-sindex lvert))
                      (site-label-state lvert)
                      (site-label-state rvert))))

        
  
(defun named-verticies-from-noffsets (noffsets)
  (mapcar #'car (apply #'append noffsets)))

(defconstant +empty-graph+ (make-complex-graph ()))
(defun lhs-default-site-binding-function (lhs-graphs)
  (let* ((bond-table (make-hash-table :test #'equal)) ; a table from site-labels to bond-labels
         (lhs-graph (case (length lhs-graphs)
                      (0 +empty-graph+)
                      (1 (first lhs-graphs))
                      (t (gtools:merge-graphs
                          (list* +empty-graph+ lhs-graphs)))))
         (lhs-labels  (graph-list-labels lhs-graphs)))
    (lambda (monomer site-num binding)
      (cond
       ((and binding (not (eq binding '*))) binding)
       (t ;; binding is nil or *
        (let* (;; the site we're trying to get info about:
               (site-label (make-site-label monomer site-num)) 
               ;; its vertex index (calculated from lhs)
               (lhs-vindex (position site-label lhs-labels :test #'named-vertex=))
               ;; 
               (existing-label  (if lhs-vindex (svref lhs-labels lhs-vindex)))
               (dindex          (position monomer lhs-labels :test #'equal)))
          (if lhs-vindex ;; if this site existed on lhs,
              (cond
               ;; a state site
               ((site-label-has-state-p existing-label) 
                (site-label-state existing-label))
               (t ; must be a connector-site
                (let* ((cnxns      (mapcar (lambda (i)
                                             (svref lhs-labels i))
                                           (remove 
                                            dindex 
                                            (gtools:graph-vertex-outputs lhs-graph lhs-vindex))))
                       (lhs-csites (list* site-label ;; all sites connected to this site on lhs
                                          cnxns))    ;; including the site itself
                                                        ;; find the bond var for any of the lhs-connected sites
                       (existing-bond-label   (firsthash lhs-csites bond-table)))

                  (cond
                   ((null cnxns)     '*)

                   (existing-bond-label existing-bond-label)

                   ((equalp (first lhs-csites) (second lhs-csites)) '_) ; "unconnected"

                   (t
                    (let ((bond-label (make-bond-label)))
                      (loop for sl in lhs-csites
                            do (setf (gethash sl bond-table) bond-label))
                      bond-label))))))
            (funcall 'non-pattern-default-site-binding monomer site-num nil))))))))
                
(defun compute-rhs-new-graph (rhs-graphs lhs-verticies)
  "The RHS-NEW-GRAPH is a graph of all monomers which are created by a reaction"
  (let ((rhs-super-graph (gtools:merge-graphs rhs-graphs)))
    (if rhs-super-graph
        (gtools:graph-delete-verticies-if 
         rhs-super-graph
         (lambda (i)
           (find (gtools:graph-vertex-label rhs-super-graph i)
                 lhs-verticies
                 :test #'named-vertex=)))
      (make-complex-graph))))

(defun graph-list-labels (glist)
  (apply #'concatenate 'simple-vector (mapcar #'gtools:graph-labels glist)))

(defun compute-complex-graph-bonds (g)
  "Returns a list of bonds between named vertex sites"
  (remove-if (lambda (x) (eq (length x) 1)) ;; should length 1 ever occur?
             ;; remove any edges to domain verticies
             (mapcar (lambda (bondlist) (remove-if #'fld-form-p bondlist))
                     ;; remove edges from domain verticies
                     (remove-if #'fld-form-p
                                (mutils:mapatoms (lambda (i)
                                                   (gtools:graph-vertex-label g i))
                                                 (gtools:graph-edges g))
                                :key #'first))))
          
(defun graph-remove-reference-labels (g)
  "Returns a copy of the complex graph by stripping any reference labels"
  (labels ((extract-true-label (lab)
             (cond
              ((fld-form-p lab) (fld-form-object lab))
              ((site-label-p lab) (mapcar #'extract-true-label lab))
              (t                  lab))))
    (let ((gcopy (gtools:copy-graph g)))
      (map-into (gtools:graph-labels gcopy)
                #'extract-true-label
                (gtools:graph-labels g))
      gcopy)))
  
(defun graph-monomers (g)
  (remove-if-not #'monomer-symbol-ref-p (gtools:graph-labels g)))

(defun monomer-localizations (rxn-var)
  "Given a localization or reference-pattern, returns an assoc list mapping monomer-ref-symbols to localizations (MREF . SUBLOC)"
  (multiple-value-bind (graph subloc)
      (if (localization-p rxn-var)
          (values rxn-var.entity.id rxn-var.location)
        (values rxn-var.id nil))
    (map 'list (lambda (msymbol) (cons msymbol subloc))
         (graph-monomers graph))))
      


;;;;
;;;; COMPUTE-COMPLEX-REACTION-TYPE-CHANGES
;;;; THIS IS A THE MAIN WORKHORSE FUNCTION WHICH COMPUTES ALL THE CHANGES DESCRIBED BY A REACTION
;;;;
(defun compute-complex-reaction-type-changes (cr)
  "Returns:  NEW-BONDS, LOST-BONDS, LABEL-CHANGES, LOST-VERTICIES, LHS-PATTERN GRAPHS, RHS-PATTERN GRAPHS, NEW-RHS-PATTERN GRAPH, LHS RXN ENTITIES, RHS MONOMER VERTEX LOCALIZATIONS"
  (mutils:let+
      (((lhs-patterns deref-lhs-patterns lhs-rxn-vars)
                         (extract-canonical-graphs-from-expression cr.lhs))
       ((rhs-patterns deref-rhs-patterns rhs-rxn-vars)
                         (extract-canonical-graphs-from-expression cr.rhs))
       (rhs-monomer-locs (mapcan #'monomer-localizations rhs-rxn-vars)) ; an assoc list of rhs monomers to sublocations
       (lhs-bonds        (mapcan #'compute-complex-graph-bonds lhs-patterns))
       (rhs-bonds        (mapcan #'compute-complex-graph-bonds rhs-patterns))
       (new-bonds        (compute-bond-set-difference rhs-bonds lhs-bonds))
       (lost-bonds       (compute-bond-set-difference lhs-bonds rhs-bonds))
       (lhs-verticies    (graph-list-labels lhs-patterns))
       (rhs-verticies    (graph-list-labels rhs-patterns))
       (rhs-new-graph    (compute-rhs-new-graph rhs-patterns lhs-verticies))
       (label-changes    (compute-label-changes lhs-verticies rhs-verticies))
       (deref-rhs-new    (graph-remove-reference-labels rhs-new-graph))) ;; deref'd new rhs graph new components

    (declare (ignorable rhs-patterns rhs-noffsets))
    (labels ((named-vertex->graph-index (label)
               ;; converts a named-vertex to a graph index (GNUM . VINDEX)
               (or (loop for graph in (list* rhs-new-graph lhs-patterns)
                         for gindex = 0 then (1+ gindex)
                         for vindex = (position label (gtools:graph-labels graph) 
                                                :test #'named-vertex=)
                         when vindex return (cons gindex vindex))
                   (error "BUG: LABEL ~S not FOUND" label)))
             (map-named-vertex->graph-index (seq)
               (map (type-of seq) #'named-vertex->graph-index seq))
             (kept-verticies () (map-named-vertex->graph-index (coerce rhs-verticies 'list)))
             (lost-verticies ()
              (map-named-vertex->graph-index
                (compute-vertex-set-difference (coerce lhs-verticies 'list)
                                               (coerce rhs-verticies 'list))))
             (named-localization->graph-index-localization (nl)
               (cons (named-vertex->graph-index (car nl))
                     (cdr nl))))

      (values 
       (mapcar #'map-named-vertex->graph-index new-bonds)  ;; new bonds
       (mapcar #'map-named-vertex->graph-index lost-bonds) ;; lost bonds
       (mapcar (lambda (lchange)                           ;; label changes
                 `(,(named-vertex->graph-index (first lchange)) ,@(rest lchange)))
               label-changes) ; label-changes
       (kept-verticies)       ; keep verticies
       (lost-verticies)       ; lose verticies
       deref-lhs-patterns     ; lhs-patterns
       deref-rhs-patterns     ; rhs-patterns
       deref-rhs-new          ; new pattern
       lhs-rxn-vars           ; complex-patterns
       (mapcar #'named-localization->graph-index-localization rhs-monomer-locs))))) ; localizations

(defun compute-rhs-graphs (input-graphs isomorphisms new-bonds lost-bonds relabels 
                                        keep lose 
                                        sublocations)
  "Returns: RHS-GRAPHS, RHS-LOCALIZATIONS
   Where RHS-GRAPHS is a list of complex-graph objects
         RHS-LOCALIZATIONS is a list of symbols representing sublocalizations
         INPUT-GRAPHS is a sequence of complex-graph objects.  The first element is
                      a graph representing all of new monomers appearing on the RHS of the reaction
                      the rest represent graphs matched on the LHS of the reaction.
         ISOMORPHISMS is a vector of isomorphisms, corresponding to one for each INPUT-GRAPH
         Graph indicies used below are cons pairs (G . I), 
           where G is the 0-based index into INPUT-GRAPHS,
             and I is the 0-based vertex in that graph
         NEW-BONDS - bonds to be created (a list of graph index pairs)
         LOST-BONDS - bonds to be destroyed (a list of graph index pairs)
         RELABELS - relabellings (representing state site value changes
                    (a list of the form (GI OLD-LABEL NEW-LABEL))
         KEEP     - RHS verticies to be kept (plus all connected verticies)
         LOSE     - verticies to be deleted from the output
                    (a list of graph indicies)
         SUBLOCATIONS - sublocations of some verticies (representing the monomers); 
                    each output graph will contain one or more denoted verticies.  
                    Each represents the sublocation that monomer should go to on the RHS.
                    Note that more than one denoted monomer may be in a complex.  If 
                    these monomers do not share the same sublocation, we have a problem.
                    This is resolved in favor of the NIL sublocation if that is present; 
                    otherwise, an error occurs (currently - fail to generate rxn in future?)."
  (let* ((graph-copies (map 'vector #'gtools:copy-graph input-graphs))
         (offsets      ()))
    (labels ((graph (n) (svref graph-copies n))
             (vertex (g i) ;; using the isomorphisms, gets the true vertex i of graph g
               (case g
                 (0 i)  ;; the 0th graph is the RHS graph - no isomorphism
                 (t (svref (svref isomorphisms (1- g)) i))))
             (offset (g) 
               (svref (or offsets
                          (apply #'vector 
                                 (loop for sum = 0 then (+ sum (gtools:graph-vertex-count graph))
                                       for graph across graph-copies
                                       collect sum)))
                      g))
             (gvertex (gi) (let ((g (car gi))) ; converts graph-index using isomorphisms
                             (cons g (vertex g (cdr gi)))))
             (svertex (gi) (let* ((gv (gvertex gi)))
                             (+ (offset (car gv))
                                (cdr gv))))
             (bond (b) (mapcar #'gvertex b))) ; converts a bond using isomorphisms

      ;; label monomer sublocations
      (loop for ((gnum . v) . subloc) in sublocations
            for graph = (graph gnum)
            for vertex = (vertex gnum v)
            do (setf (gtools:graph-vertex-label graph vertex)
                     #[localization (gtools:graph-vertex-label graph vertex)
                                    subloc]))

      ;; disconnect edges
      (loop for ((gnum . v1) (nil . v2)) in lost-bonds
            do (setf (gtools:graph-vertex-edge-p (graph gnum) 
                                                 (vertex gnum v1)
                                                 (vertex gnum v2))
                     nil))

      ;; relabel verticies
      (loop for ((g . v) nil to) in relabels
            do (setf (site-label-state (gtools:graph-vertex-label 
                                        (graph g)
                                        (vertex g v)))
                     to))

      ;; merge the graphs, connect edges between them, and delete verticies
      (let* ((rhs-super-graph (gtools:merge-graphs (coerce graph-copies 'list) 
                                                  :edges (mapcar #'bond new-bonds)))
             ;; and return the distinct complexes resulting from this operation:
             ;; keeping only the RHS verticies: 
             (disconnected-graphs (gtools:unconnected-subgraphs rhs-super-graph :verticies (mapcar #'svertex keep))))
        (if lose (mapcar #'fix-bond-sites disconnected-graphs)
          disconnected-graphs)))))

(defun fix-bond-sites (cg)
  "Connects bond sites which have no bonds (other than to the monomer symbol) to themselves.
   This is necessary when monomers have been destroyed, leaving unconnected verticies."
  (loop for i from 0 below (gtools:graph-vertex-count cg)
        for lab = (gtools:graph-vertex-label cg i)
        when (and (site-label-p lab)
                  (not (site-label-has-state-p lab))
                  (= 1 (length (gtools:graph-vertex-outputs cg i))))
        do (setf (gtools:graph-vertex-edge-p cg i i) t)
        finally (return cg)))
