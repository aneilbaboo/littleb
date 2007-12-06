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

;;;; EXAMPLE:
;;;; (with-substitution-table 
;;;;                  ((a b c)
;;;;                   (1 2 3)
;;;;                   (4 5 6))
;;;;                (format t "~A ~A ~A~%" a b c))
;;;; (PROGN
;;;;  (FORMAT T "~A ~A ~A~%" 1 2 3)
;;;;  (FORMAT T "~A ~A ~A~%" 4 5 6))

(defmacro with-data-table ((&key row col data) (colnames &rest rows) &body body)
  "Treats nil and _ as empty. "
  (let ((sub-table  (mapcan
                     (lambda (rowlist)
                       (loop with rowname = (first rowlist)
                             for colname in colnames
                             for d in (rest rowlist)
                             when (and d (not (eq d '_)))
                             collect (if (listp data) (list* colname rowname d)
                                       (list colname rowname d))))
                     rows)))
    (if (listp data)
        `(with-substitution-table 
             ((,col ,row ,@data)
              ,@sub-table)
           ,@body)
      `(with-substitution-table 
           ((,col ,row ,data)
            ,@sub-table)
         ,@body))))

;;;; EXAMPLE
;;;; (with-data-table (:row V1! :col V2! :data D!)
;;;;     ((   'a 'b 'c)
;;;;      ('a  1  2  3)
;;;;      ('b  4  5  6)
;;;;      ('c  7  8  9))
;;;;   (format t "~S ~S ~S~%" v1! v2! D!))
       
          
(defmacro with-substituted-combinations (substitution-sets &body body)
  "Where Bindings = ((v1 m1 m2 m3 ..)
                     (v2 m4 m5 m6 ...)
                     ...)"
  (let* ((cpaths (mapcar (lambda (b) ;; (((v1 m1) (v1 m2) (v1 m3))
                                     ;;  ((v2 m4) (v2 m5) (v2 m6))...)
                           (let ((v (first b)))
                             (mapcar (lambda (m)
                                       (list v m))
                                    (rest b))))
                         substitution-sets))
         (subst-bindings  (mutils:compute-choice-paths cpaths)))
    `(with-substitutions ,subst-bindings ,@body)))

;;;; FOR EXAMPLE:
;;;;
;;;; (with-monomer-combinations ((r erb1 erb2)
;;;;                             (l egf nrg))
;;;;   {[r] + [l] ->> [[r 1][l 1]]}
;;;;   {[[r 1][l 1]] ->> [r] + [l]})
;;;;
;;;; =M>
;;;;
;;;; (PROGN
;;;;  {[ERB1] + [EGF] ->> [[ERB1 1] [EGF 1]]}
;;;;  {[[ERB1 1] [EGF 1]] ->> [ERB1] + [EGF]}
;;;;  {[ERB1] + [NRG] ->> [[ERB1 1] [NRG 1]]}
;;;;  {[[ERB1 1] [NRG 1]] ->> [ERB1] + [NRG]}
;;;;  {[ERB2] + [EGF] ->> [[ERB2 1] [EGF 1]]}
;;;;  {[[ERB2 1] [EGF 1]] ->> [ERB2] + [EGF]}
;;;;  {[ERB2] + [NRG] ->> [[ERB2 1] [NRG 1]]}
;;;;  {[[ERB2 1] [NRG 1]] ->> [ERB2] + [NRG]})


(defmacro macro-multilet ((header &rest data) form)
  (let ((forms (mapcar (lambda (row)
                         `(destructuring-bind ,header ',row
                            ,form)) data)))
    `(progn ,@forms)))


#+:lispworks (editor:setup-indent 'macro-multilet 2 2)

