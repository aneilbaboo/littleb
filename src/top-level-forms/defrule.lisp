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

;;; File: defrule
;;; Description: defrule, defines b rules
;;;

;;; $Id: defrule.lisp,v 1.8 2008/09/02 14:58:11 amallavarapu Exp $
;;;
(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(portable:define-dspec-class defrule nil nil)

(define-field-form-parser defrule)

(defun gentempn (&optional name (num 0) (package *package*))
  (multiple-value-bind (symbol exists) (intern (format nil "~A~A" name num) package)
    (if exists (gentempn name (1+ num))
      symbol)))
      
(defmacro with-add-rule (name &body body)
  (let ((pattern         '#:pattern)
        (form            '#:form)
        (rule-name       '#:rule-name)
        (user-name       '#:user-name))
    `(flet ((ADD-RULE (,pattern ,form &optional ,user-name)
              (let ((*print-pretty* nil)
                    (,rule-name (or ,user-name (gentempn (format nil "~A-DYNRULE" ',name) 0 ,*package*))))
                (eval `(defrule ,,rule-name ,,pattern => ,,form)))))
       ,@body)))
            
(defmacro defrule (name &body args); patterns => &body body)
  (let+ (((doc-str 
           patterns
           => 
           body)           (if (stringp (first args))
                               (values (first args) (second args) (third args) (cdddr args))
                             (values nil (first args) (second args) (cddr args))))
         (rule-parse      (make-rule-parse patterns))
         (rhs-fn-symbol   (let ((*print-pretty* nil)) (sym name "-RHS-FN")))
         (lambda-list     (rule-parse-rhs-lambda-list rule-parse))
         (rhs-code        `(funcall ',rhs-fn-symbol ,@(rule-parse-rhs-apply-args rule-parse)))
         (lisa-defs       (rule-parse-lisa-rule-definitions rule-parse name rhs-code))
         (cvar            (gensym "CLASS")))
    (unless (and name (symbolp name)) 
      (b-error "Invalid rule name ~S." name))
                              
    (unless (eq => '=>)
      (b-error "Defining rule ~A: ~A was provided where => was expected." name =>))
    
    `(eval-when (#+:lispworks :compile-toplevel :load-toplevel :execute)
       (portable:dspec (defrule ,name)
         (reload-on-reset)
         (defun ,rhs-fn-symbol ,@lambda-list
           (with-add-rule ,name ,@body))
       
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (define-global ,name (make-rule ',name ',patterns (list ,@lisa-defs)
                                           ',rhs-fn-symbol ',doc-str))
           
           (dolist (,cvar ',(rule-parse-classes rule-parse))
             (kb-register-matchable-class ,cvar) t)
           
           (kb-transaction))
       
         ',name))))

#+:allegro
(defmethod make-load-form ((r rule) &optional env)
  (declare (ignorable env))
  (with-slots (symbol patterns rhs-fn documentation) r
    (let* ((parse (make-rule-parse patterns))
           (rhs-code        `(,rhs-fn ,@(rule-parse-rhs-apply-args parse)))
           (lisa-defs       (rule-parse-lisa-rule-definitions parse name rhs-code)))
         
      `(make-rule ',symbol ',patterns (list ,@lisa-defs)
                  ',rhs-fn ',documentation))))

(defmethod documentation ((r rule) doc-type)
  (declare (ignorable doc-type))
  (rule-documentation r))
  
(port:setup-indent 'defrule 1 2 9 t)
)



