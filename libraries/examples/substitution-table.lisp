;;; 
;;; Substitution-tables are tabular structures organized as a series of 
;;; rows, which enables substitutions to be performed on a body of code
;;; for rows in the table.  The first row, the header, specifies 
;;; variables which will be substituted.  The subsequent rows specify
;;; values which will be substituted into the variables, and used to 
;;; generate a body of code.
;;;
;;; (WITH-SUBSTITUTION-TABLE (header data-row1 data-row2 ...) code)
(in-package :b-user)

;; include non-dimensional library
(include b-user/ode-biochem) 

;; Examples of use of with-substitution-table

;; NOTE: The $ in front of variable names is 
;; only a convention to indicate that a symbol
;; is being used as a substitution variable
;; within the body of the macro

;; define 6 molecules with similar structure:
(with-substitution-table 
    ;; here is the table - the header row is the symbol $NAME
    ($NAME mon1 mon2 mon3 mon4 mon5 mon6)

  ;; and here is the code:
  (defmonomer $NAME A B C)) ; each has 3 binding sites

;; define some dimerization reactions: 
(with-substitution-table 

    ;; first argument is a table of substitutions
    (($M1    $M2    $Kon    $Koff) ; header row is a list of 4 elements
     (mon1   mon2   1       1)     ; the following rows must be lists of 4 elements
     (mon3   mon1   .5      .3)     
     (mon2   mon6   3       4)
     (mon1   mon5   2       .5))

  ;; and here is the code:
  {[$M1 A._ **] + [$M2 A._ **] <<->>
   [[$M1 A.1 **][$M2 A.1 **]]}.
  (set-rate-function 'mass-action :fwd $Kon :rev $Koff))

     