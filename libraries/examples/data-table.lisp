;;; 
;;; Datatables are two-dimensional structures organized as rows and columns
;;; where cells provide data which characterizes the relation between the
;;; row element and the column element.
;;;
;;; (WITH-DATA-TABLE bindings table code)
;;;
;;; The with-data-table macro iterates over every cell of the TABLE and binds
;;; the row and column labels for that cell, as well as the cell data to 
;;; the variables provided in the BINDINGS section.  On each iteration,
;;; a body of code is produced with the substitutions indicated by each cell.
;;;
(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) 

;; see receptor-ligand.lisp for background on this model

;;; define 3 receptors with similar structure:
(defmonomer (receptor1 membrane) L D C)
(defmonomer (receptor2 membrane) L D C)
(defmonomer (receptor3 membrane) L D C)

;; two ligands
(defmonomer ligand1 R)
(defmonomer ligand2 R)

;;;
;;; ligands 1 and 2 bind differentially to the receptors
;;; 
(with-data-table (:rows $R :cols $L :cells ($Kon $Koff) :ignore _) ; ignore "_" cells

    ;; the table cells are the rate constants which describe how 
    ;; the receptor (column) binds and unbinds to the ligand (row)
    ((          receptor1   receptor2   receptor3)
     (ligand1   (.1 .1)     (1  .5)     _        ) ; doesn't bind receptor3
     (ligand2   _           (1  .5)     (.4 .7)  )) ; doesn't bind receptor1

  ;; and here is the code which describes the reaction:

  {[$R L._ **] + [L _] @ :outer <<->> [[$R L.1][L 1]]}.
   (set-rate-function 'mass-action :fwd $Kon :rev $Koff))



;;;
;;; receptor dimerization: L-R1 + R2 <-> L-R1-R2
;;;
(with-data-table (:rows ($R1 $L) :cols $R2 :cells ($Kon $Koff) :ignore _)

    ;; again, the table cells hold the kinetic data
    ;; note that the row labels contain 2 elements ($R1 and $L), 
    ;; the receptor and ligand it's bound to
    ;; whereas the column label contains only $R2
    ((                     receptor1    receptor2   receptor3)
     ((receptor1 ligand1)  (1 .3)       (1 1)       (.5 .5))
     ((receptor2 ligand1)  (.01 .2)     (.3 .3)     (.5 .6))
     ((receptor2 ligand2)  (.5 .5)      (.5 .7)     (.2  .4))
     ((receptor3 ligand2)  (.3  .8)     (1.3 4)     (4  .5)))

  ;; and here's the code which describes the reaction:

  {[[$R1 L.1 D._ **][$L 1]] + [$R2 D._ **] 
   <<->> 
   [[$R1 L.1 D.2 **][$L 1][$R2 D.2 **]]}.
    (set-rate-function 'mass-action :fwd $Kon :rev $Koff))
