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

;;; File: define
;;; Description: defines a global symbol, S, which is used as the parent value of
;;;              *name* when generating new objects.  The object returned by the
;;;              form takes (id S) as its ID.

;;; $Id: define.lisp,v 1.2 2008/05/12 19:42:52 amallavarapu Exp $
;;;
;;;
(in-package b)

(portable:define-dspec-class define nil nil)
(define-field-form-parser define)

(defmacro define (specifier value &environment env)
  (let+ (((name exposure) (parse-name-exposure specifier))
         (form            `(progn (process-name-exposure ',name ',exposure)
                             ,value)))
    (cond 
     ((const-fld-form-p name)    (let* ((symbol    (fld-form-to-symbol name))
                                        (def-form  (make-typed-define-form name name form env)))
                                   (compute-define-symbol-code name symbol def-form)))
     
     ((symbolp name)             (let* ((place       (make-global-place-symbol name))
                                        (def-form    (make-typed-define-form name place form env)))
                                   (compute-define-symbol-code name name def-form)))

     (t                          (error "Invalid name provided to DEFINE: ~S" name)))))


(defun expose-name (x &optional (default-exposure :expose))
  "Checks whether a name needs exposure, exposes it - this is an API function"
  (multiple-value-bind (name exposure)
      (parse-name-exposure x default-exposure)
    (process-name-exposure name exposure)))


(defun process-name-exposure (symbol exposure)
  (when exposure
    (dolist (upkg (package-transitive-used-by-list *package*))
      (safely-remove-conflicting-internal-symbols symbol upkg)))
  (ecase exposure
    (:expose (expose-symbol symbol))
    (:export (export symbol))
    (:internal ())))

;;;; (defun process-name-exposure (name exposure)
;;;;   (labels ((symbol-from-name (x)
;;;;              (cond ((and (consp x) (eq (first x) 'setf))
;;;;                     (symbol-from-name (second x)))
;;;;                    ((fld-form-p x)
;;;;                     (fld-form-to-symbol x))
;;;;                    ((consp x) (apply #'delim-sym #\- x))
;;;;                    (t         x))))
;;;;     (let ((symbol (symbol-from-name name)))      
;;;;       (when exposure
;;;;         (dolist (upkg (package-transitive-used-by-list *package*))
;;;;           (safely-remove-conflicting-internal-symbols symbol upkg)))
;;;;       (ecase exposure
;;;;         (:expose (expose-symbol symbol))
;;;;         (:export (export symbol))
;;;;         (:internal ())))))                                         

(defun exposure-directive-p (o)
  (case o ((:expose :export :internal) t)))

(defun parse-name-exposure (specifier &optional (default-exposure :expose))
  "Returns 2 values: name, exposure (:EXPOSE :EXPORT or :INTERNAL)"
  (labels ((symbol-from-name (x)
             (cond ((and (consp x) (eq (first x) 'setf))
                    (symbol-from-name (second x)))
                   ((fld-form-p x)
                    (fld-form-to-symbol x))
                   ((consp x) (apply #'delim-sym #\- x))
                   (t         x))))
  (cond 
   ((and (consp specifier)
         (exposure-directive-p (first specifier)))
    (values (symbol-from-name (second specifier)) (first specifier)))

   (t (values (symbol-from-name specifier) default-exposure)))))

(defun make-typed-define-form (name place form env)
  (let* ((type    (compute-form-type form env))
         (tx-form (if (constantp form) form
                    `(kb-transaction 
                       (setf ,place (has-name ,name ,form))))))
    (if type `(the ,type ,tx-form)
      tx-form)))

(defun compute-define-symbol-code (name symbol form)
  (let ((global-def  `(define-global ,symbol ,form)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (define ,name)
         
         (update-global-modular-location ',symbol)

         (kb-transaction ,global-def)

         ',name))))

(defun parse-function-name (name)
  (cond
   ((consp name) (if (eq 'setf (first name))
                     (second name)
                   (error "Unexpected function name: ~S." name)))
   (t            name)))

;;;; (eval-when (:compile-toplevel :execute)
;;;; (defmacro with-exposable-define-form ((specifier dspec) &body macro-body)
;;;;   `(let+ (((name exposure) (parse-name-exposure ,specifier))
;;;;           (dspec            ,dspec)
;;;;           (dspec-class      (first dspec)))
;;;;      `(eval-when (:compile-toplevel :load-toplevel :execute)
;;;;         (portable:define-dspec-class ,dspec-class () nil)
;;;;         (portable:dspec ,dspec
;;;;           (process-name-exposure ',name ',exposure)
;;;;           (portable:record-definition ',dspec (portable:location) t)
;;;;           ,,@macro-body)))))

;;;; (defmacro define-function (&whole whole specifier lambda-list &body body)
;;;;   (declare (ignoreable lambda-list body))
;;;;   (with-exposable-define-form (specifier `(define-function ,specifier))
;;;;     `(defun ,@(rest whole))))

(defmacro define-function (specifier lambda-list &body body)
  (let+ (((name exposure) (parse-name-exposure specifier)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (define-function ,name)
         (process-name-exposure ',name ',exposure)
         (defun ,name ,lambda-list ,@body)))))

(defmacro define-generic (specifier lambda-list &body options)
  (let+ (((name exposure) (parse-name-exposure specifier)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (defgeneric ,name)
         (process-name-exposure ',name ',exposure)
         (defgeneric ,name ,lambda-list ,@options)))))

(defmacro define-method (specifier &body body)
  (let+ (((name exposure) (parse-name-exposure specifier)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (process-name-exposure ',name ',exposure)
       (defmethod ,name ,@body))))

(defmacro define-macro (specifier lambda-list &body body)
  (let+ (((name exposure) (parse-name-exposure specifier)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (process-name-exposure ',name ',exposure)
       (defmacro ,name ,lambda-list
         ,@body))))

(portable:define-dspec-class define-var () ())
(defmacro define-var (&whole whole specifier &optional initial-value documentation)
  (declare (ignorable initial-value documentation))
  (let+ (((name exposure) (parse-name-exposure specifier))
         (dspec           `(define-var ,specifier))
         (location        (portable:location)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (define-var ,specifier)
         (portable:record-definition ',dspec ',location t)
         (process-name-exposure ',name ',exposure)
         (defvar ,@(rest whole))))))

(portable:define-dspec-class define-parameter () ())
(defmacro define-parameter (&whole whole specifier initial-value &optional documentation)
  (declare (ignorable initial-value documentation))
  (let+ (((name exposure) (parse-name-exposure specifier))
         (dspec           `(define-parameter ,specifier))
         (location        (portable:location)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (define-var ,specifier)
         (portable:record-definition ',dspec ',location t)
         (process-name-exposure ',name ',exposure)
         (defparameter ,@(rest whole))))))

(portable:define-dspec-class define-constant () ())
(defmacro define-constant (&whole whole specifier initial-value &optional documentation)
  (declare (ignorable initial-value documentation))
  (let+ (((name exposure) (parse-name-exposure specifier))
         (dspec           `(define-constant ,specifier))
         (location        (portable:location)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (portable:dspec (define-var ,specifier)
         (portable:record-definition ',dspec ',location t)
         (process-name-exposure ',name ',exposure)
         (defconstant ,@(rest whole))))))
