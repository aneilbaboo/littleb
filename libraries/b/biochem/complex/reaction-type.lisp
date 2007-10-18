
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

;;; $Id: reaction-type.lisp,v 1.4 2007/10/18 00:13:41 amallavarapu Exp $
;;; $Name:  $

;;; File: complex-reaction-type.lisp
;;; Description: defines the complex-species-type structure
;;;              relies on the graph-tools library

(in-package :b-user)

(include (b/math @folder/species-type))

(include-declaration :expose-symbols '(<<->> ->>))

(defoperator ->> ((+ 1 (operator-precedence '+)) :xfy :macro)
    (lhs rhs)
  (let ((lhsvar '#:lhs)
        (rhsvar '#:rhs))
  `(with-complex-reaction-type-parts ((,lhsvar ,lhs) (,rhsvar ,rhs))
;     (values ,lhsvar ,rhsvar))))
     [complex-reaction-type ,lhsvar ,rhsvar])))


(defcon reference-pattern (complex-graph-concept :notrace)
  "Reference patterns are created when reactions are defined; each monomer has a reference number indicating its identity in the context of a complex-reaction-type"
  (id)
  (setf .id (ensure-canonical-complex-graph id t)))


;;;; (defmethod print-concept ((o reference-pattern) &optional stream)
;;;;   (let ((graph     o.id))
;;;;     (print-complex-graph graph stream "[" "]")))

(defcon complex-pattern (reference-pattern :notrace)
  "Selector patterns are asserted into the database and detected by the detect-complex-pattern-isomorphism rule"
  (id)
  (setf .id (ensure-canonical-complex-graph id nil)))

(defcon complex-reaction-type ()
  (lhs 
   rhs)
  (setf .lhs (canonicalize-complex-reaction-type-argument lhs)
        .rhs (canonicalize-complex-reaction-type-argument rhs)))

(defmethod print-concept ((o complex-reaction-type) &optional stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression o stream t)))

(defmethod print-math-expression ((o complex-reaction-type) &optional (stream *standard-output*) (outer-op t))
  (pprint-math-form `{,o.lhs ->> ,o.rhs} stream outer-op))


(defun canonicalize-complex-reaction-type-argument (x)
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
               (localization   [localization (canonicalize-object x.reactants) x.sublocation])
               (monomer         (eval `[,x.name]))
               (complex-species-type [reference-pattern x.id])
               (reference-pattern x))))
    {(canonicalize-object x)}))

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
  (etypecase x
    (list          (mapcar (lambda (elt) 
                             (typecase elt
                              (localization elt.type.id)
                              (reference-pattern elt.id)
                              (t (b-error "Invalid input to complex-reaction-type: ~S" elt))))
                           x))
    (sum-expression (graphs-in-expression x.vars))
    (t              (graphs-in-expression (list x)))))
                           

(defun extract-canonical-graphs-from-expression (expr)
  (let ((graphs (graphs-in-expression expr)))
    (loop for g in graphs
          for (cg r) = (multiple-value-list (gtools:canonical-graph (graph-remove-reference-labels g)))
          for rg = (gtools:reorder-graph g r)
          collect cg into dereferenced-graphs
          collect rg into reference-graphs
          finally return (values reference-graphs dereferenced-graphs))))


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
      (((lhs-patterns deref-lhs-patterns)
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
       deref-rhs-new))))

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


;;;;
;;;;
;;;; CODE FOR TESTING AND MISC JUNK CODE:
;;;;
;;;; ;;;;

;;;; (defun compute-reaction-output (lhs rhs lhs-graphs)
;;;;   "For testing purposes: Given lhs and rhs pattern graphs, and a set of lhs-graphs, computes the rhs graph"
;;;;   (multiple-value-bind (lhs-patterns rhs-patterns new-rhs-graph connects disconnects label-changes remove)
;;;;       (parse-complex-reaction-type2 lhs rhs)
;;;;     (declare (ignorable rhs-patterns))
;;;;     (let* ((lhs-graphs   (mapcar #'make-complex-graph lhs-graphs))
;;;;            (isomorphisms (apply #'mapcar #'vector
;;;;                                 (mapcar #'gtools:find-subgraph-isomorphisms 
;;;;                                         lhs-patterns
;;;;                                         lhs-graphs))))
;;;;         (loop for isoset in isomorphisms
;;;;               for rhs-graphs = (compute-rhs-graphs (apply #'vector new-rhs-graph lhs-graphs)
;;;;                                                    isoset
;;;;                                                    connects
;;;;                                                    disconnects 
;;;;                                                    label-changes 
;;;;                                                    remove)
;;;;               collect rhs-graphs))))

;;;; (defcon reversible-complex-reaction-type () 
;;;;   (lhs rhs)
;;;;   {.rxns := (order rxns)})

;;;; (defun make-reversible-complex-reaction (lhs rhs)
;;;;   [reversible-complex-reaction (list [complex-reaction lhs rhs]
;;;;                                      [complex-reaction rhs lhs])])


;;;; (defun plus-op-p (x)
;;;;   (and (consp x)
;;;;        (eq (first x) '+op)))

;;;; (defun mult-op-p (x)
;;;;   (and (consp x)
;;;;        (eq (first x) '*op)))

;;;; (defun math-op-p (x)
;;;;   (or (plus-op x) (mult-op x)))

;;;; OLD PARSE-COMPLEX-REACTION2
;;;; (defun parse-complex-reaction2 (lhs rhs)
;;;;   "On input, lhs and rhs are patterns (as returned from pattern-from-complex-reaction-argument)"
;;;;   (mutils:let+
;;;;       (((lhs-patterns lhs-cnxns) 
;;;;         (parse-complex-reaction-single-side-pattern lhs))
;;;;        (*default-site-binding* (lhs-default-site-binding-function lhs-patterns))
;;;;        ((rhs-patterns rhs-cnxns)
;;;;         (parse-complex-reaction-single-side-pattern 
;;;;          (pattern-from-complex-reaction-argument rhs)))
;;;;        (created-cnxns  (compute-bond-difference rhs-cnxns lhs-cnxns))
;;;;        (lost-cnxns     (compute-bond-difference lhs-cnxns rhs-cnxns))
;;;;        (lhs-verticies  (graph-list-labels lhs-patterns))
;;;;        (rhs-verticies  (graph-list-labels rhs-patterns))
;;;;        (rhs-new-graph  (compute-rhs-new-graph rhs-patterns lhs-verticies))
;;;;        (label-changes    (compute-label-changes lhs-verticies rhs-verticies)))
;;;;     (declare (ignorable rhs-patterns rhs-noffsets))
;;;;     (labels ((named-vertex->graph-index (label)
;;;;                ;; converts a named-vertex to a graph index (GNUM . VINDEX)
;;;;                (or (loop for graph in (list* rhs-new-graph lhs-patterns)
;;;;                          for gindex = 0 then (1+ gindex)
;;;;                          for vindex = (position label (gtools:graph-labels graph) 
;;;;                                                 :test #'named-vertex=)
;;;;                          when vindex return (cons gindex vindex))
;;;;                    (error "BUG: LABEL ~S not FOUND" label)))
;;;;              (map-named-vertex->graph-index (seq)
;;;;                (map (type-of seq) #'named-vertex->graph-index seq))
;;;;              (lost-verticies ()
;;;;               (map-named-vertex->graph-index
;;;;                 (compute-vertex-set-difference (coerce lhs-verticies 'list)
;;;;                                                (coerce rhs-verticies 'list)))))

;;;;       (values 
;;;;        (mapcar #'graph-remove-reference-labels lhs-patterns) ;; lhs-patterns
;;;;        (mapcar #'graph-remove-reference-labels rhs-patterns) ;; rhs-patterns
;;;;        (graph-remove-reference-labels rhs-new-graph) ;; new rhs graph new components
;;;;        (mapcar #'map-named-vertex->graph-index created-cnxns) ;; created cnxns
;;;;        (mapcar #'map-named-vertex->graph-index lost-cnxns) ;; lost cnxns
;;;;        (mapcar (lambda (lchange)                           ;; label changes
;;;;                  `(,(named-vertex->graph-index (first lchange)) ,@(rest lchange)))
;;;;                label-changes)
;;;;        (lost-verticies)))))
;;;;             



;;;;        


;;;; (defun unpack-complex-reaction-argument (x)
;;;;   "Returns a list of object forms"
;;;;   (cond
;;;;    ((null x) nil)
;;;;    ((math-form-p x)
;;;;     (unpack-complex-reaction-argument (math-form-expand x)))

;;;;    ((mult-op-p x)
;;;;     (let ((num (second x))
;;;;           (obj (third x)))
;;;;       (unless (and (numberp num) (plusp num)
;;;;                    (complex-form-p obj))
;;;;         (error "Expecting NUMBER OBJECT-PATTERN"))
;;;;       (unpack-complex-reaction-argument `(+op ,@(make-list num :initial-element obj)))))
;;;;    ((plus-op-p x)
;;;;     (mapcan #'unpack-complex-reaction-argument (rest x)))
;;;;    ((symbolp x)
;;;;     (unpack-complex-reaction-argument `([[,x]])))
;;;;    ((object-form-p x)
;;;;     (if (object-form-p (object-form-object x))
;;;;         `(,x)
;;;;       `([,x])))
;;;;    ((listp x)
;;;;     (mapcan #'unpack-complex-reaction-argument x))))



;;;; (defun pattern-from-complex-reaction-argument (x)
;;;;   "Takes an expression like {ksr + mapk} or {[[ksr.a !1 _][mapk.b !1 *]] + [mek.c _]}
;;;;    and returns a list of complexes with named monomers:
;;;;    (((ksr.',(ksr 0) _ _)) ((mapk.',(mapk 0) _ _))) and
;;;;    (((ksr.a !1 _ _)(mapk.b !1 *)) ((mek.c _ :u)))"
;;;;   (let ((ref-table (make-hash-table :test #'equal)) ;; the ref table is 
;;;;         (monomer-table (make-hash-table)))
;;;;     (labels ((new-monomer-binding (d)
;;;;                (list d (incf (gethash d monomer-table 0))))
;;;;              (check-monomer-reference (d)
;;;;                (let ((r (fld-form-field d)))
;;;;                  (unless (atom r) (error "Invalid reference ~S - expecting an atom." d))
;;;;                  (if (gethash r ref-table) 
;;;;                      (error "Duplicate reference ~A." r)
;;;;                    (setf (gethash r ref-table) t))
;;;;                d))
;;;;              (parse-monomer (d)
;;;;                (cond
;;;;                 ((fld-form-p d) (check-monomer-reference d))
;;;;                 (t              `.d.,(new-monomer-binding d))))
;;;;              (parse-complex (cplex)
;;;;                (mapcar (lambda (dform)
;;;;                          `(,(parse-monomer (object-form-object dform))
;;;;                            ,@(object-form-args dform)))
;;;;                        (object-form-body cplex))))
;;;;       (mapcar #'parse-complex (unpack-complex-reaction-argument x)))))



;;;; (defun parse-complex-pattern (cplx)
;;;;   "Given P, a pattern,
;;;;    Returns: G graph representing the pattern, 
;;;;                suitable for isomorphism testing
;;;;             C, a bond list"
;;;;   (multiple-value-bind (slabels binding-table)
;;;;       (parse-complex-description cplx t)
;;;;     (fix-automatic-bond-labels binding-table)
;;;;     (loop with graph = (build-complex-graph slabels binding-table)
;;;;           for i from 0 below (gtools:graph-vertex-count graph)
;;;;           for lab = (gtools:graph-vertex-label graph i)
;;;;           finally return (values graph
;;;;                                  (compute-bond-list binding-table slabels)))))
;;;;     
;;;; (defun parse-complex-reaction-type-single-side-pattern (arg)
;;;;   (loop for cplx in arg
;;;;         for (graph clist)
;;;;                    = (multiple-value-list (parse-complex-pattern cplx))
;;;;         collect (gtools:canonical-graph graph) into graphs
;;;;         append clist into bonds
;;;;         finally return (values graphs bonds)))
;;;;                

;;;; (defun compute-site-value-changes (lhs rhs)
;;;;   "Given two lists of site labels, determines what site values have changed"
;;;;   (loop for xsite in lhs
;;;;         for ysite = (find xsite rhs :test #'named-vertex=)
;;;;         for rhs = (remove ysite rhs)
;;;;         unless (or (null ysite)
;;;;                    (equal (site-label-value xsite)
;;;;                           (site-label-value ysite)))
;;;;         collect ysite))

