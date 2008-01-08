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

;;; File: initialize
;;; Description: a set of defmacro-like macros which do raw substitutions on 
;;;              code (without the use of backquote operator)
;;;
(in-package :mallavar-utility)

;;;;
;;;; 
;;;;
(defmacro with-substitutions (binding-sets &body body)
  "A binding-set is a list of variable-value pair lists:
     ((v1 l1) (v2 l2)...);  binding-sets is a list of such lists.
    Each binding-set is used to substitute the variables (Vi) 
    with the corresponding values (Li) everywhere in BODY"
  (let ((subst-bodies (mapcan
                       (lambda (substs)
                         (mutils:mapatoms 
                          (lambda (atom)
                            (let ((sub (assoc atom substs)))
                              (if sub (second sub)
                                atom)))
                          body))
                       binding-sets)))
    `(progn ,@subst-bodies)))

;;;; FOR EXAMPLE:
;;;; (with-substitutions
;;;;     (((x [erb _]) (y [egf _]) (xy [[erb 1][egf 1]])
;;;;       (k 1.5))
;;;;      ((x [erb _]) (y [nrg _])(xy [[erb 1][nrg 1]])
;;;;       (k .001)))  
;;;;   [{x + y ->> xy} (.set-rate-function 'mass-action k)]))
;;;; =M>
;;;; (PROGN
;;;;  [{[ERB _] + [EGF _] ->> [[ERB 1] [EGF 1]]}
;;;;   (|.SET-RATE-FUNCTION| 'MASS-ACTION 1.5)]
;;;;  [{[ERB _] + [NRG _] ->> [[ERB 1] [NRG 1]]}
;;;;   (|.SET-RATE-FUNCTION| 'MASS-ACTION 0.001)])

;;;; WITH-SUBSTITUTION-TABLE:
;;;; User supplies a table of variables and substitutions for those
;;;; variables.  The block of code following the table is 
;;;; EXAMPLE:
;;;; (with-substitution-table 
;;;;                  ((a b c)
;;;;                   (1 2 3)
;;;;                   (4 5 6))
;;;;                (format t "~A ~A ~A, " a b c))
;;;; =M>
;;;; (PROGN
;;;;  (FORMAT T "~A ~A ~A, " 1 2 3)
;;;;  (FORMAT T "~A ~A ~A, " 4 5 6))
;;;; =>
;;;; 1 2 3, 4 5 6

(defmacro with-substitution-table ((header &rest data) &body body)
  "The table consists of a header (symbols/column names) and data, a 
   set of lists containing the values corresponding to each column.
   Each row of data is substituted into body everywhere the corresponding
   column header appears."    
  (let ((binding-sets (mapcar (lambda (row)
                                (mapcar #'list
                                        (if (listp header) header (list header))
                                        (if (listp row) row (list row))))
                              data)))
    `(with-substitutions ,binding-sets
       ,@body)))

;;;; WITH-DATA-TABLE:  
;;;; User specifies:
;;;;    * substitution variables
;;;;    * 2D-table of columns and rows
;;;;    * body of code
;;;; For each cell in the table, the macro results
;;;; in a substituted version of the code where the
;;;; column headers and row headers as well as data
;;;; are substituted everywhere the variables appear.
;;;; 
;;;; The user can specify a value which signals a cell 
;;;; should be ignored.  By default this is NIL.
;;;;
;;;; 
               
;;;; EXAMPLE
;;;; (with-data-table (:row V1! :col V2! :data D!)
;;;;     ((   'a 'b 'c)
;;;;      ('a  1  2  3)
;;;;      ('b  4  5  6)
;;;;      ('c  7  8  9))
;;;;   (format t "~S ~S ~S, " v1! v2! D!))
;;;; =>
;;;  A A 1, A B 1, A C 3, B A 4, B B 5 ...
       
(defmacro with-data-table ((&key row col data ignore) (colnames &rest rows) &body body)
  "ROW COL and DATA are substituted in the code (BODY).  If a cell contains the ignore symbol,
   no substitutions will be performed.  By default, IGNORE is NIL.
   ROW, COL and DATA may be either a symbol or a list of symbols.  
   In the later case, the corresponding element in the table must be a list of the same length."
  (let* ((row        (or row (gensym "row")))
         (col        (or col (gensym "col")))
         (data       (or data (gensym "data")))
         (sub-table  (mapcan
                      (lambda (rowlist)
                        (loop with rowname = (first rowlist)
                              for colname in colnames
                              for d in (rest rowlist)
                              unless (eq d ignore)
                              collect `(,@(if (listp col) colname (list colname))
                                        ,@(if (listp row) rowname (list rowname))
                                        ,@(if (listp data) d (list d)))))
                     rows)))
    `(with-substitution-table 
         ((,@(ensure-list col) ,@(ensure-list row) ,@(ensure-list data))
          ,@sub-table)
           ,@body)))


(defun compute-permuted-substitutions (substitution-sets)
  (let ((cpaths (mapcar (lambda (b) ;; (((v1 m1) (v1 m2) (v1 m3))
                          ;;  ((v2 m4) (v2 m5) (v2 m6))...)
                          (let ((v (first b)))
                             (mapcar (lambda (m)
                                       (list v m))
                                     (rest b))))
                        substitution-sets)))
    (mutils:compute-choice-paths cpaths)))

;; WITH-PERMUTATIONS:          
(defmacro with-permutations (substitution-sets &body body)
  "Where Bindings = ((v1 m1 m2 m3 ..)
                     (v2 m4 m5 m6 ...)
                     ...)"
  `(with-substitutions ,(compute-permuted-substitutions substitution-sets) ,@body))

(defmacro with-combinations (substitution-sets &body body)
  "Calculates all 
  (let* ((psets (compute-permuted-substitutions substitution-sets))
         (csets (remove-duplicates 
                 psets 
                 :test (lambda (x y)
                         (null (set-exclusive-or (mapcar #'cdr x) (mapcar #'cdr y) :test #'equal))))))
    `(with-substitutions ,csets ,@body)))
    
;;;; FOR EXAMPLE:
;;;;
;;;; (with-permutations ((a 1 2)
;;;;                     (b 1 2))
;;;;   (format t "~A ~A~%" a b))
;;;; =>
;;;; 1 1
;;;; 1 2
;;;; 2 1
;;;; 2 2

;;;; (with-combinations ((a 1 2)
;;;;                     (b 1 2))
;;;;   (format t "~A ~A~%" a b))
;;;; =>
;;;; 1 1
;;;; 1 2
;;;; 2 2

(defmacro macro-multilet ((header &rest data) form)
  (let ((forms (mapcar (lambda (row)
                         `(destructuring-bind ,header ',row
                            ,form)) data)))
    `(progn ,@forms)))


#+:lispworks (editor:setup-indent 'macro-multilet 2 2)

