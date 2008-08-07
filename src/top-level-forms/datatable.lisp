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
(in-package :b)

;;;;
;;;; 
;;;;
(defmacro with-substitutions (binding-sets &body body)
  "A binding-set is a list of variable-value pair lists:
     ((v1 l1) (v2 l2)...);  binding-sets is a list of such lists.
    Each binding-set is used to substitute the variables (Vi) 
    with the corresponding values (Li) everywhere in BODY"
  (flet ((at-symbol-p (x) (and (symbolp x)
                               (char= #\@ 
                                      (char (symbol-name x) 0)))))
    (let ((subst-bodies (mapcan
                         (lambda (substs)
                           (mutils:mapatoms 
                            (lambda (atom)
                              (let* ((assoc    (assoc atom substs))
                                     (var      (first assoc))
                                     (sub      (second assoc))
                                     (splicep  (at-symbol-p atom)))
                                (cond
                                 (assoc 
                                  (when (and splicep 
                                             (not (and (listp sub)
                                                       (not (fld-form-p sub)))))
                                    (error "Invalid substitution ~S for splice variable ~S" 
                                           sub var))
                                  (values sub
                                          splicep))
                                 (t atom))))
                            body))
                         binding-sets)))
      `(progn ,@subst-bodies))))

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
                                        (if (listp header) row (list row))))
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
;;;; (with-data-table (:cols V1 :rows V2 :cells v3)
;;;;     ((   'a 'b 'c)
;;;;      ('d  1  2  3)
;;;;      ('e  4  5  6)
;;;;      ('f  7  8  9))
;;;;   (format t "~S ~S ~S, " v1 v2 v3))
;;;; =>
;;;; A D 1, B D 2, C D 3, A E 4, B E 5, C E 6, A F 7, B F 8, C F 9, 
       
(defmacro with-data-table ((&key rows cols cells ignore) (colnames &rest rowdata) &body body)
  "ROWS COLS and DATA are substituted in the code (BODY).  If a cell contains the ignore symbol,
   no substitutions will be performed.  By default, IGNORE is NIL.
   ROWS, COLS and DATA may be either a symbol or a list of symbols.  
   In the later case, the corresponding element in the table must be a list of the same length."
  (flet ((destructure-arguments (params args)
           (cond
            ((listp params) 
             (unless (and (listp args) 
                          (eq (length args) (length params)))
               (error "WITH-DATA-TABLE arguments do not match parameters. ~
                       Expecting values for ~S, ~
                       but received ~S." params args))
             args)
            (t (list args)))))
    (let* ((rows        (or rows (gensym "rows")))
           (cols        (or cols (gensym "cols")))
           (cells       (or cells (gensym "cells")))
           (sub-table  (mapcan
                        (lambda (rowlist)
                          (loop with rowname = (first rowlist)
                                for colname in colnames
                                for cell in (rest rowlist)
                                unless (eq cell ignore)
                                collect `(,@(destructure-arguments cols colname)
                                          ,@(destructure-arguments rows rowname)
                                          ,@(destructure-arguments cells cell))))
                        rowdata)))
    `(with-substitution-table 
         ((,@(ensure-list cols) ,@(ensure-list rows) ,@(ensure-list cells))
          ,@sub-table)
           ,@body))))


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
  "Substitution-sets = ((V1 S11 S12 ..S1n) (V2 S21 ...S2n) ...)
   Where Body is evaluated for each permutation possible 
     where Vi is substituted for Sij"
  `(with-substitutions ,(compute-permuted-substitutions substitution-sets) ,@body))

(defmacro with-combinations (substitution-sets &body body)
  "Substitution-sets = ((V1 S11 S12 ..S1n) (V2 S21 ...S2n) ...)
   Where Body is evaluated for each combination possible 
     where Vi is substituted for Sij"
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

