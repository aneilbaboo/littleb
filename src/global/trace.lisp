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

;;; File: trace
;;; Description: support for tracing events in the knowledge base

;;; $Id: trace.lisp,v 1.2 2007/11/21 07:10:57 amallavarapu Exp $

;;;
(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct (trace-info (:constructor new-trace-info (event data)))
  event data)

(defstruct (rule-trace-info (:include trace-info)
                            (:constructor _new-rule-trace-info 
                             (event data bindings stack-location)))
  bindings stack-location)


(defun trace-objects (&rest opts)
  "opts = either t, NIL, or verbose; or a keyword list with keys :NEW :DELETE :UPDATE :RULES"
  (macrolet ((update-if-not-missing (var val)
               `(unless (missingp ,val)
                  (setf ,var ,val))))
    (handler-case 
        (flet ((set-vars (&key (new ?) (delete ?) (update ?) (rules ?))
                 (update-if-not-missing *kb-trace-new-objects* new)
                 (update-if-not-missing *kb-trace-delete-objects* delete)
                 (update-if-not-missing *kb-trace-update-objects* update)
                 (update-if-not-missing *kb-trace-rules* rules)))

          (let ((first (first opts)))
            (cond ((position first '(t nil :verbose))
                   (set-vars :new first :delete first :update first :rules first))

                  ((missingp first) nil) ;; if missing is supplied, then return the current trace vars

                  (t (apply #'set-vars opts))))

          (list :new *kb-trace-new-objects*
                :delete *kb-trace-delete-objects*
                :update *kb-trace-update-objects*
                :rules *kb-trace-rules*))
      (error (e) (declare (ignore e))
        (error "Invalid input to trace-objects ~S - expecting T, NIL or :VERBOSE, or a keyword list containing any of :NEW :DELETE :UPDATE :RULES" opts)))))

(defun add-trace-info (ti)
  (push ti (first *trace-stack-location*)))

(defmacro add-object-trace (event object)
  (let ((o (gensym "OBJECT")))
    `(let ((,o ,object))
       (when (and ,(trace-event-to-var-map event)
                  (traceable-object-p ,o))
         (add-trace-info (new-trace-info ,event ,o))))))


(defun add-rule-trace (rule bindings)
  (cond 
   ((and *kb-trace-rules* (traceable-object-p rule))
    (let ((rti (new-rule-trace-info rule bindings)))
      (add-trace-info rti)
      (let ((*trace-stack-location* (rule-trace-info-stack-location rti)))
        (apply (rule-rhs-fn rule) bindings))))
   (t (apply (rule-rhs-fn rule) bindings))))

;;;; (defmacro with-trace-pprint-dispatch (&body body)
;;;;   `(let ((*print-pprint-dispatch* (with-standard-io-syntax (copy-pprint-dispatch))))
;;;;      (set-pprint-dispatch 'trace-info #'print-trace-info 0)
;;;;      (set-pprint-dispatch 'rule-trace-info #'print-rule-trace-info 1)
;;;;      ,@body))

(defun flush-trace (&optional (stream *standard-output*))
  (when (trace-stack-has-items-p)
    (fresh-line stream)
    (let ((stack                   (retrieve-trace-stack)))
      (mapcar (lambda (ti)
                (pprint-logical-block (stream nil)
                  (princ ti stream)))
              stack)
      (clear-trace-stack))))

;#+CLISP
;(progn
(defmethod print-object ((ti trace-info) stream)
  (print-trace-info stream ti))
(defmethod print-object ((rti rule-trace-info) stream)
  (print-rule-trace-info stream rti)) ;)

(defun traceable-object-p (o)
  (not (hidden-p o)))

(defun print-trace-info (stream ti)
  (flet ((print-obj (event obj verbose?)
           (with-print-context (not verbose?)
             (pprint-logical-block (stream nil)
               (format stream "(~(~A~)) " event)
               (prin1 obj stream)))
           (pprint-newline :mandatory stream))
         (print-prop (event prop verbose?)
           (declare (ignorable verbose?))
           (let* ((pclass  (class-of prop))
                  (obj     (property-object prop))
                  (prop-id `(fld ,obj ,(pclass-field-symbol pclass)))
                  (prop-val (property-value prop)))
             (pprint-logical-block (stream nil)
               (format stream "(~(~A~)) " event)
                (print-name `(fld ,(property-object prop)
                                   ,(pclass-field-symbol pclass))
                          stream)
               (cond
                ((equalp prop-id (object-name prop-val))
                 (princ " :#= " stream)
                 (print-concept prop-val stream))
                (t (princ " := " stream)
                   (prin1 prop-val stream)))
               (pprint-newline :mandatory stream)))))
    (let* ((event   (trace-info-event ti))
           (data    (trace-info-data ti))
           (verbose (case event
                      (:new (eq :verbose *kb-trace-new-objects*))        
                      (:delete (eq :verbose *kb-trace-delete-objects*))
                      (:update (eq :verbose *kb-trace-update-objects*)))))
      (cond ((propertyp data)
             (if (not (and (eq event :new) *kb-trace-new-objects*))
                 (print-prop event data verbose)))
            ((conceptp data)            
             (print-obj event data (if (eq event :new) t)))
            (t (error "BUG-INVALID OBJECT TRACE INFO ~A" ti))))))
   
(defun trace-event-to-var-map (event)
  (ecase event
    (:update   '*kb-trace-update-objects*)
    (:new      '*kb-trace-new-objects*)
    (:delete   '*kb-trace-delete-objects*)
    (:rule     '*kb-trace-rules*)
    (:rule-end '*kb-trace-rules*)))
  
(defun new-rule-trace-info (rule-name pvar-args)
  (_new-rule-trace-info :rule 
                        (global-value rule-name) 
                        (map-plist (lambda (k v) (list k v))
                                   pvar-args)
                        (list (list))))

(defun print-rule-trace-info (stream rti)
  (let* ((rule          (rule-trace-info-data rti))
         (symbol        (rule-symbol rule))
         (patterns      (rule-patterns rule))
         (bindings      (rule-trace-info-bindings rti))
         (stack         (retrieve-trace-stack (rule-trace-info-stack-location rti))))
      (if (eq *kb-trace-rules* :verbose)
          (format stream "(rule) ~<~S ~:_~<~A~:> ~:_where~{~< ~A = ~S~:>; ~:@_~}=> ~[~S~;(no new objects)~%~]~{~:@_~S~}~:>"
                  (list symbol 
                        patterns
                        bindings
                        (if stack 0 1)
                        (first stack)
                        (rest stack)))
        (format stream "(rule) ~<~S =>~[~:@_~S~; (no new objects)~%~]~{~S~}~:>~%"
                (list symbol 
                      (if stack 0 1)
                      (first stack)
                      (rest stack))))))

(defconstant *trace-objects-options* '(:new :delete :update :rules))

(defun set-class-visibility (obj visible)
  (if visible
      (remhash obj *hidden-classes*)
    (setf (gethash obj *hidden-classes*) t))

  (when (cclassp obj)    
    (mapcar (lambda (o)
              (set-class-visibility o visible))
            (append ;(mapcar #'rule-symbol (cclass-rules obj)) - cclass-rules removed
                    (cclass-properties obj)))))

(defun hide-classes (&rest classes)
  (mapcar (lambda (class)   
            (set-class-visibility class nil))
          classes))

(defun unhide-classes (&rest classes)
  (cond 
   ((eq classes t)
    (clrhash *hidden-classes*))
   (t 
    (mapcar (lambda (c) 
              (set-class-visibility c t))
            classes)))
  t)
)


;;;; (defun traceable-object-p (o)
;;;;   (or (and (conceptp o)
;;;;            (not (gethash (class-of o) *untraceable-classes*)))
;;;;       (and (propertyp o)
;;;;            (not (gethash (class-of o) *untraceable-classes*))
;;;;            (traceable-object-p (property-value o)))
;;;;       (and (rule-p o)
;;;;            (not (gethash o *untraceable-classes*)))))
