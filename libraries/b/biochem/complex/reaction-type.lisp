
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

;;; $Id: reaction-type.lisp,v 1.6 2007/10/25 03:58:00 amallavarapu Exp $
;;; $Name:  $

;;; File: complex-reaction-type.lisp
;;; Description: defines the complex-species-type structure
;;;              relies on the graph-tools library

(in-package #I@library/biochem)

(include (b/math @folder/species-type @library/biochem/reaction-type))

(defoperator ->> ((+ 1 (operator-precedence '+)) :xfy :macro)
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
  (let* ((lhs (canonicalize-complex-reaction-type-argument lhs))
         (rhs (canonicalize-complex-reaction-type-argument rhs))
         (location-class (determine-complex-reaction-type-location-class location-class lhs rhs)))
    (setf .lhs lhs
          .rhs rhs
          .location-class location-class)))


(defun determine-complex-reaction-type-location-class (defined-lclass lhs rhs)
  (flet ((find-location-class-in-sum-expression (se)
            (mutils:ifit (find-if #'complex-graph-concept-p (if se se.vars))
                         it.location-class)))
    (let* ((lclass (or defined-lclass 
                       (find-location-class-in-sum-expression lhs)
                       (find-location-class-in-sum-expression rhs))))
      (check-complex-reaction-type-argument lhs lclass)
      (check-complex-reaction-type-argument rhs lclass)
      lclass)))

(defun check-complex-reaction-type-argument (rt-arg loc-class)
  (labels ((coef-ok (num)
             (eq num 1))
           (check-term (var num)
             (typecase var
               (complex-graph-concept   
                (unless (subtypep var.location-class loc-class)
                  (b-error "~S.location-class does not match ~S." var loc-class)))
               (localization
                (unless var.(is-valid-for loc-class)
                  (b-error "Invalid reaction-type argument: ~S.  ~
                            No ~S sublocation in location class ~S." 
                           var  var.location loc-class)))
               (t            
                (b-error "Expecting a complex-pattern or location-requirement, but received ~S." var)))
             (unless (coef-ok num)
               (b-error "Invalid stoichiometry (~S) in reaction-type sum-expression ~S" num rt-arg))))
    (when rt-arg 
      (unless (sum-expression-p rt-arg)
        (b-error "Invalid argument to complex-reaction-type: ~S." rt-arg))
      rt-arg.(map-terms #'check-term))))

(defmethod print-concept ((o complex-reaction-type) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression o stream t)))

(defmethod print-math-expression 
           ((o complex-reaction-type)
            &optional (stream *standard-output*) (outer-op t))
  (pprint-math-form `{,o.lhs ->> ,o.rhs} stream outer-op))


(defun canonicalize-complex-reaction-type-argument (side)
  (labels ((canonicalize-object (x) 
             (etypecase x 
               (sum-expression (apply #'s+
                                      x.(map-terms 
                                         (lambda (o c) 
                                           (unless (= 1 c) 
                                             (b-error "Coefficient invalid in ~A ~S as complex reaction argument"
                                                      c o))
                                           (canonicalize-object o)))))
               (list           (apply #'s+ (mapcar #'canonicalize-object x)))
               (localization   [localization (canonicalize-object x.entity) x.location])
               (monomer         (eval `[,x.name]))
               (complex-species-type [reference-pattern x.id])
               (reference-pattern x))))
    (canonicalize-object side)))

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
     
(defun graphs-in-expression (x)
  "Returns complex-graph objects plus the corresponding vars in the expression"
  (etypecase x
    (list           (loop for elt in x
                          collect elt into rxn-vars
                          collect (typecase elt
                                    (localization elt.entity.id)
                                    (reference-pattern elt.id)
                                    (t (b-error "Invalid input to complex-reaction-type: ~S" elt)))
                          into graphs
                         finally return (values graphs rxn-vars)))
    (sum-expression (graphs-in-expression x.vars))
    (t              (graphs-in-expression (list x)))))
                           

(defun extract-canonical-graphs-from-expression (expr)
  (mutils:let+ (((graphs rxn-vars) (graphs-in-expression expr)))
    (loop for g in graphs
          for (cg r) = (multiple-value-list (gtools:canonical-graph (graph-remove-reference-labels g)))
          for rg = (gtools:reorder-graph g r)
          collect cg into dereferenced-graphs
          collect rg into reference-graphs
          finally return (values reference-graphs dereferenced-graphs rxn-vars))))


;;;
;;; Named bond - a pair (c1 c2) where c1 and c2 are vertex names
;;;                    (either a fld-form MONOMER.REF 
;;;                         site ref (SITE-INDEX MONOMER.REF &optional VALUE)
;;; 
(defun compute-bond-list (binding-table site-labels)
  "Returns a list of sites and a list of "
  (loop with sites = (apply #'vector site-labels)
        for k being the hash-key in binding-table
        for v being the hash-value in binding-table
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
  "If we have generated auto-generated bonds vars which have only one bond side,
   convert these to self-bond (_) bonds."
  (loop for v being the hash-key in binding-table
        for cnxns being the hash-value in binding-table
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
        when (and rvert
                  (not (equal (site-label-value rvert)
                              (site-label-value lvert))))
        collect (list (make-site-label (site-label-monomer lvert)
                                       (site-label-sindex lvert))
                      (site-label-value lvert)
                      (site-label-value rvert))))

        
  
(defun named-verticies-from-noffsets (noffsets)
  (mapcar #'car (apply #'append noffsets)))

(defun lhs-default-site-binding-function (lhs-graphs)
  (let* ((rev-table (make-hash-table :test #'equal)) ; a table from site-labels to 
         (lhs-graph (case (length lhs-graphs)
                      (0 #.(gtools:make-labelled-graph ()()))
                      (1 (first lhs-graphs))
                      (t (gtools:merge-graphs
                          (list* #.(gtools:make-labelled-graph ()()) lhs-graphs)))))
         (labels    (graph-list-labels lhs-graphs)))
    (lambda (monomer site-num binding)
      (cond
       ((and binding (not (eq binding '*))) binding)
       (t
        (let* ((site-label (make-site-label monomer site-num))
               (vindex     (position site-label labels :test #'named-vertex=))
               (existing-label      (if vindex (svref labels vindex)))
               (dindex     (position monomer labels :test #'equal)))
          (if vindex
              (cond
               ((site-label-has-value-p existing-label)
                (site-label-value existing-label))
               (t ; must be a connector-site
                (let* ((cnxns      (mapcar (lambda (i)
                                             (svref labels i))
                                           (remove 
                                            dindex 
                                            (gtools:graph-vertex-outputs lhs-graph vindex))))
                       (lhs-csites (list* site-label ;; all sites connected to this site on lhs
                                          cnxns))
                       (existing   (firsthash lhs-csites rev-table)))
                  (cond
                   ((null cnxns)     '*)
                   ((equalp existing `(,monomer ,site-num)) '_) ; connected to self
                   (t
                    (let ((cnxn-var (make-bond-label)))
                      (loop for sl in lhs-csites
                            do (setf (gethash sl rev-table) cnxn-var))
                      cnxn-var))))))
            (funcall 'non-pattern-default-site-binding monomer site-num binding))))))))
                
(defun compute-rhs-new-graph (rhs-graphs lhs-verticies)
  "The RHS-NEW-GRAPH is a graph of all monomers which are created by a reaction"
  (let ((rhs-super-graph (gtools:merge-graphs rhs-graphs)))
    (gtools:graph-delete-verticies-if 
     rhs-super-graph
     (lambda (i)
       (find (gtools:graph-vertex-label rhs-super-graph i)
             lhs-verticies
             :test #'named-vertex=)))))

(defun graph-list-labels (glist)
  (apply #'concatenate 'simple-vector (mapcar #'gtools:graph-labels glist)))

(defun compute-complex-graph-bonds (g)
  "Returns a list of bonds between named vertex sites"
  (remove-if (lambda (x) (eq (length x) 1))
             (mapcar (lambda (bondlist) (remove-if #'fld-form-p bondlist))
                     (mutils:mapatoms (lambda (i)
                                        (gtools:graph-vertex-label g i))
                                      (gtools:graph-edges g)))))


(defun make-complexes (graphs)
  (mapcar (lambda (g)
            [complex-species-type g])
          graphs))

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
  

(defield reaction-type.changes ()
  "Returns:  NEW-BONDS, LOST-BONDS, LABEL-CHANGES, LOST-VERTICIES, LHS-PATTERNS, RHS-PATTERNS, NEW-RHS-PATTERN"
  (multiple-value-bind (bonds lost-bonds label-changes lhs-patterns rhs-patterns new-rhs-graph)
      (compute-complex-reaction-type-changes object)
    (values bonds lost-bonds label-changes 
            [complex-pattern lhs-patterns]
            [complex-pattern rhs-patterns]
            [complex-pattern new-rhs-graph])))

(defun compute-complex-reaction-type-changes (cr)
  "Returns:  NEW-BONDS, LOST-BONDS, LABEL-CHANGES, LOST-VERTICIES, LHS-PATTERN GRAPHS, RHS-PATTERN GRAPHS, NEW-RHS-PATTERN GRAPH"
  (mutils:let+
      (((lhs-patterns deref-lhs-patterns lhs-rxn-vars)
                         (extract-canonical-graphs-from-expression cr.lhs))
       ((rhs-patterns deref-rhs-patterns)
                         (extract-canonical-graphs-from-expression cr.rhs))
       (lhs-cnxns        (mapcan #'compute-complex-graph-bonds lhs-patterns))
       (rhs-cnxns        (mapcan #'compute-complex-graph-bonds rhs-patterns))
       (created-cnxns    (compute-bond-set-difference rhs-cnxns lhs-cnxns))
       (lost-cnxns       (compute-bond-set-difference lhs-cnxns rhs-cnxns))
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
             (lost-verticies ()
              (map-named-vertex->graph-index
                (compute-vertex-set-difference (coerce lhs-verticies 'list)
                                               (coerce rhs-verticies 'list)))))

      (values 
       (mapcar #'map-named-vertex->graph-index created-cnxns) ;; new bonds
       (mapcar #'map-named-vertex->graph-index lost-cnxns) ;; lost bonds
       (mapcar (lambda (lchange)                           ;; label changes
                 `(,(named-vertex->graph-index (first lchange)) ,@(rest lchange)))
               label-changes)
       (lost-verticies)
       deref-lhs-patterns
       deref-rhs-patterns
       deref-rhs-new
       lhs-rxn-vars))))

(defun compute-rhs-graphs (lhs-graphs isomorphisms bonds disbonds relabels remove)
  (let* ((lhs-graphs (map 'simple-vector #'gtools:copy-graph lhs-graphs)))
    (labels ((graph (n) (svref lhs-graphs n))
             (vertex (g i) (case g
                             (0 i)  ;; the 0th graph is the RHS graph - no isomorphism
                             (t (svref (svref isomorphisms (1- g)) i))))
             (gvertex (gi) (let ((g (car gi)))
                             (cons g (vertex g (cdr gi)))))
             (bond (c) (mapcar #'gvertex c)))
      
      ;; disconnect edges
      (loop for ((gnum . v1) (nil . v2)) in disbonds
            do (setf (gtools:graph-vertex-edge-p (graph gnum) 
                                                 (vertex gnum v1)
                                                 (vertex gnum v2))
                     nil))

      ;; relabel verticies
      (loop for ((g . v) nil to) in relabels
            do (setf (site-label-value (gtools:graph-vertex-label 
                                        (graph g) 
                                        (vertex g v)))
                     to))

      ;; merge the graphs, connect edges between them
      (let ((rhs-super-graph (gtools:merge-graphs (coerce lhs-graphs 'list) 
                                                  :edges (mapcar #'bond bonds)
                                                  :remap `((nil ,@(mapcar #'gvertex remove))))))
        ;; and return the distinct complexes resulting from this operation:
        (gtools:unconnected-subgraphs rhs-super-graph)))))

