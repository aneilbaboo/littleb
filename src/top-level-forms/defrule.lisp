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

;;; File: defrule
;;; Description: defrule, defines b rules
;;;

;;; $Id: defrule.lisp,v 1.6 2008/01/22 16:42:40 amallavarapu Exp $
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
  
#+Lispworks
(editor:setup-indent 'defrule 1 2 9 t)
)



