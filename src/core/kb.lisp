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

;;; $Id: kb.lisp,v 1.8 2008/02/14 22:33:34 amallavarapu Exp $
;;; $Name:  $

(in-package b)
    
(defvar *kb-monitors* ()
  "A list of single-argument functions which are called when an object is added to the database.")

(defun kb-type-count ()
  (loop with table = (make-hash-table)
        for o being the hash-values of +objects+
        do (incf (gethash (type-of o) table 0))
        finally return (maphash-to-list table)))
        
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
  (dolist (monitor *kb-monitors*) (funcall monitor object))
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

(defun verbose-tracer (o)
  (cond 
   ((traceable-object-p o) (format t "~&(new) ~S~%" o))
   (t                      (princ #\+ o))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun quiet-tracer (&optional (n *monitor-interval*))
  "Prints a + for every N new objects."
  (let ((counter 0))
    (flet ((quiet-trace (o)
             (declare (ignore o))
             (when (zerop (mod (incf counter) n))
               (princ #\+)
               #+:clisp
               (force-output))))
      #'quiet-trace))))

(defun memory-monitor (&optional (n *monitor-interval*))
  (let ((counter 0))
    (flet ((memory-trace (o)
             (declare (ignore o))
             (when (zerop (mod (incf counter) n))
               (room))))
      #'memory-trace)))

(defun all-tracer (o)
  "Prints a verbose trace of every object added to the DB"
  (format t "~&(new) ~S~%" o))

(defun traceable-object-p (o)
  (let ((class (class-of o)))
    (and (subtypep (type-of class) 'kb-class)
       (kb-class-traceable-p class))))

(defun difference-tracer (&key (step 100) test)
  (flet ((type-count-difference (tc1 tc2)
           (loop for (t1 . c1) in tc1
                 for (nil . c2) = (or (assoc t1 tc2) '(nil . 0))
                 when (> c1 c2)
                 collect (list t1 (- c1 c2)))))
    (let ((instance-counter 0)
          (type-count (kb-type-count))
          (type-count-new ()))
      (flet ((difference-trace (o)
               (incf instance-counter)
               (if test (funcall test o))
               (when (= (mod instance-counter step) 0)
                 (setf type-count-new (kb-type-count))
                 (format t "~&New entities after ~S steps:~%~
                     ~{~S +~S~~%~}"
                         (type-count-difference type-count-new type-count))
                 (setf type-count type-count-new))))
        #'difference-trace))))

(setf *kb-monitors* (list (quiet-tracer)))


;;;; ;;; CCLASS-CREATE-FROM-HASHKEY - added to support reify-concept
;;;; (defun cclass-create-from-hashkey (hashkey)
;;;;   (let ((cclass   (first hashkey))
;;;;         (id-vals  (rest hashkey)))
;;;;     (cclass-create-instance
;;;;      cclass
;;;;      (loop for id-fld in (cclass-id-field-order cclass)
;;;;            for val    in id-vals
;;;;            nconc (list id-fld val)))))

