;;;; This file is part of little b.

;;;; Copyright (c) 2005-8 Aneil Mallavarapu

;;;; Little b is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Little b is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with little b.  If not, see <http://www.gnu.org/licenses/>.

;;; File: define
;;; Description: defines a global symbol, S, which is used as the parent value of
;;;              *name* when generating new objects.  The object returned by the
;;;              form takes (id S) as its ID.

;;; $Id: define.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $
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
