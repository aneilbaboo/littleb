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

;;; File: kb
;;; Description: Mostly internal functions for dealing with the knowledge base

;;; $Id: kb.lisp,v 1.13 2008/08/20 17:16:47 amallavarapu Exp $
;;; $Name:  $

(in-package b)
    
(defvar *kb-monitor* ()
  "A single-argument function which is called when an object is added to the database.")

(defun kb-type-count ()
  (loop with table = (make-hash-table)
        for o being the hash-values of +objects+
        do (incf (gethash (type-of o) table 0))
        finally (return (maphash-to-list table))))
        
(defun kb-find-object (hashkey)
  (gethash hashkey +OBJECTS+))

(defun kb-add-object (hashkey object)
  (let ((class (class-of object)))
    (cond
     ;; object is matched by a pattern:
     ((kb-class-matchable-p class)        
      (push (cons :add object) *kb-command-queue*)) 

     ;;; store this instance in the unmatched list
     (t (push object (kb-class-unmatched-instances class))))
    (setf (gethash hashkey +objects+) object
          (kb-object-hashkey object) hashkey)
    (unless (pclassp class) (funcall *kb-monitor* object))
    object))

(Defun kb-unwind-object (hashkey object)
  (remhash hashkey +objects+)
  (setf *kb-command-queue* (delete object *kb-command-queue* :key #'cdr)))

(defun kb-request-run ()
  (unless *kb-command-queue*
    (push (cons :request-run nil) *kb-command-queue*)))

(defun kb-register-matchable-class (class)
  (unless (kb-class-matchable-p class)
    (mapc #'kb-register-matchable-class (portable:class-direct-subclasses class))
    (push (cons :add-class class) *kb-command-queue*)
    (setf (slot-value class 'matchable-p) t)))

(defun kb-update-object (object)
  (when (kb-class-matchable-p (class-of object))
    (push (cons :update object) *kb-command-queue*))
  object)


(defun kb-run ()
  (when *kb-command-queue*
    (loop with transaction = ()
          while *kb-command-queue*
          do (setf transaction (nreverse *kb-command-queue*)
                   *kb-command-queue* nil)
          when transaction
          do (loop do (kb-do-command (pop transaction)) 
                    while transaction)
          	 (lisa:run))))

(defun kb-do-command (cmd)
  (let ((action (car cmd))
        (object (cdr cmd)))
    (ecase action
      (:update      (lisa:mark-instance-as-changed object))
      (:add         (kb-do-add-command object))
      (:add-class   (kb-add-unmatched-instances object))
      (:request-run ()))))

(defun kb-add-unmatched-instances (class)
  (loop for object = (pop (kb-class-unmatched-instances class))
        while object
        do (lisa:assert-instance object)))

(defun kb-do-add-command (object)
  (lisa:assert-instance object)
  object)

(defun kb-reset ()
  (maphash (lambda (k v)
             (declare (ignorable k))
             (when (conceptp v)
               (setf (kb-object-hashkey v) nil)))
           +OBJECTS+)
  (clrhash +OBJECTS+)
  (setf *kb-command-queue* nil)
  (lisa:clear)
  (lisa:reset)
  t)

(defun kb-retract (o)
  (let* ((hashkey (concept-hashkey o)))
    (cond ((gethash hashkey +OBJECTS+)  
           (setf (gethash hashkey +OBJECTS+) nil)
           (lisa:retract-instance o)
           t)
          (t nil))))

                
(defun kb-finalize-property (prop)
  (let ((*relevance* t))
    (setf (fld (property-object prop) (pclass-field-symbol (class-of prop)))
          (property-value prop))))

;;;
;;;  MONITORING SYSTEM:
;;;

(defvar *monitor-interval* 10
  "Used by some monitors - # of instances seen before a report is issued.")

(defstruct (type-signal-table
            (:constructor %make-type-signal-table 
             (indicators signals)))
  indicators signals)
(defun make-type-signal-table (type-signals)
  "Provides a table which enables kb-monitor fns to quickly determine what action to 
   take given an object. 
   Type-cmds is a list of conses, where CAR = indicator, CDR = signal
   On input, indicator is a string or a symbol representing a type
    - eg b/math:sum-expression
         \"b/math:sum-expression\"
         \"b/math::sum-expression\"
         \"sum-expression\".
   The first 3 indicators match the type SUM-EXPRESSION in the package B/MATH,
   whereas the last indicator matches SUM-EXPRESSION in any package.
   The SIGNAL is provided by the fn which creates the kb-monitor, and is usually just T."
  (let* ((signals  (make-hash-table :test #'equalp))
         (tinds    (mapcar (lambda (typesig)
                             (let ((ind (compute-type-indicator (car typesig)))) ; comput the indicator
                               (setf (gethash ind signals) (cdr typesig))        ; associate indicator with signal
                               ind))
                           type-signals)))
    (%make-type-signal-table tinds signals)))

(defun compute-type-indicator (ind)
  "Returns a cons pair of strings (PACKAGE . SYMBOL-NAME) representing a symbol 
   or just a string for the symbol name"
  (etypecase
   (string
    (let ((symbol-elts  (mapcar #'string-upcase 
                                (remove-if #'zerop (tok '(#\:) ind) :key #'length))))
      (case (length symbol-elts)
        (1 (first symbol-elts))
        (2 (apply #'cons symbol-elts))
        (t (error "Invalid input: ~S" ind)))))
   (symbol (symbol-components ind))
   (cons (symbol-components (first ind)))))
  
(defun indicator-typep (o indicator)
  "Given an object, and an indicator, determines whether o is of the type specified by indicator"
  (let ((type (typify-indicator indicator)))
    (if type (ignore-errors (typep o type)))))

(defun typify-indicator (ind)
  (etypecase ind
    (cons   (if (find-package (car ind))
                (find-symbol (cdr ind) (car ind))
              'null))
    (string `(or ,@(remove 'null (mapcar (lambda (p) (typify-indicator (cons ind p)))
                                         (list-all-packages)))))))

(defun symbol-components (symbol &optional shortp)
  (if shortp (symbol-name symbol)
    (cons (package-name (symbol-package symbol))
          (symbol-name symbol))))

(defun get-object-type-signal (o ttable)
  "Given an object and a type-table, returns the type signal for that object"
  (let+ ((class           (class-of o))
         (signals         (type-signal-table-signals ttable))
         ((signal found)  (gethash class signals)))
    (cond
     (found  signal)
     (t      (let* ((ind (find-if (lambda (ind) (indicator-typep o ind))
                                  (type-signal-table-indicators ttable))))
               (setf (gethash class signals) 
                     (gethash ind signals)))))))
             
(defun type-signalify (indicators signal)
  (mapcar (lambda (ind) (cons ind signal)) indicators))

(defun make-verbose-kb-monitor (&key quiet-types)
  "Shows all objects"
  (let ((ttable (make-type-signal-table (type-signalify quiet-types t))))
    (labels ((verbose-kb-monitor (o)
               (if (get-object-type-signal o ttable)
                   (format t "~&(new) ~S~%" o)
                 (princ #\+ *standard-output*)))))
    #'verbose-kb-monitor))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-quiet-kb-monitor (&key (n *monitor-interval*) ignore-types)
  "Prints a + for every N new objects."
  (let ((counter 0)
        (ttable (make-type-signal-table (type-signalify ignore-types t))))
    (flet ((quiet-kb-monitor (o)
             (when (and (null (get-object-type-signal o ttable))
                        (zerop (mod (incf counter) n)))
               (princ #\+)
               #+:clisp
               (force-output))))
      #'quiet-kb-monitor))))

(defun make-memory-kb-monitor (&optional (n *monitor-interval*))
  "returns a monitor which calls (ROOM) every N objects"
  (let ((counter 0))
    (flet ((memory-kb-monitor (o)
             (declare (ignore o))
             (when (zerop (mod (incf counter) n))
               (room))))
      #'memory-kb-monitor)))

(defun all-kb-monitor (o)
  "Prints a verbose trace of every object added to the DB"
  (format t "~&(new) ~S~%" o))
 
(defun traceable-object-p (o)
  (let ((class (class-of o)))
    (and (subtypep (type-of class) 'kb-class)
       (kb-class-traceable-p class))))

(defun make-difference-kb-monitor (&key (step 100) test)
  "Calculates the difference in the number of objects of a particular type every STEP inferences.  
   If test is given, then only objects which satisfy TEST are counted"
  (flet ((type-count-difference (tc1 tc2)
           (loop for (t1 . c1) in tc1
                 for (nil . c2) = (or (assoc t1 tc2) '(nil . 0))
                 when (> c1 c2)
                 collect (list t1 (- c1 c2)))))
    (let ((instance-counter 0)
          (type-count (kb-type-count))
          (type-count-new ()))
      (flet ((difference-kb-monitor (o)
               (incf instance-counter)
               (if test (funcall test o))
               (when (= (mod instance-counter step) 0)
                 (setf type-count-new (kb-type-count))
                 (let ((entities  (type-count-difference type-count-new type-count)))
                   (if entities
                       (apply #'format t "~&New entities after ~S steps:~%~
                     ~{~S +~S~%~}"
                              entities)))
                 (setf type-count type-count-new))))
        #'difference-kb-monitor))))

(setf (symbol-function 'default-kb-monitor) (make-quiet-kb-monitor :ignore-types '("sum-expression")))
(setf *kb-monitor* 'default-kb-monitor)
