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

;;; File: field-pattern
;;; Description: enables definition of field-pattern expressions (for use in rules)

;;; $Id: field-pattern.lisp,v 1.2 2007/11/21 07:10:56 amallavarapu Exp $
;;; $Name:  $

(in-package :b)
           
(defgeneric field-pattern (type field &optional args)
  (:documentation "This generic function returns a list of patterns"))

(defmacro defield-pattern (fe llist &body body)
  "This macro stores a function which returns two values: a pattern, and (optionally)
   the variable which binds the value represented by the field-expression pattern."
  (let+ (((doc body)  (if (stringp (first body)) (values (first body) (rest body))
                        (values nil body)))
         (type       (fld-form-object fe))
         (fld         (fld-form-field fe))
         (fi          (class-fieldinfo (find-class (fld-form-object fe))
                                       (fld-form-field fe)))
         (s-llist     (fieldinfo-args fi)) ; stored lambda list
         (type-param   (gensym "TYPE"))
         (field-param   (gensym "FIELD"))
         (args-param   (gensym "ARGS")))
    (declare (ignorable doc))
    (assert (matching-lambda-lists-p llist s-llist) ()
      "Expecting lambda list ~S to match stored lambda list ~S." llist s-llist)
    `(progn
       (defmethod field-pattern ((,type-param (eql ',type)) (,field-param (eql ',fld))
                                 &optional ,args-param)
         (destructuring-bind (,@llist) ,args-param
           ,@body)))))
    
(defun rule-parse-read-fld-method-pattern (rule-parse fe val-var)
  (let+ ((obj (fld-form-object fe))
         (type (if (pattern-var-p obj) (rule-parse-pvar-type rule-parse obj)
                 (prog1 obj (rule-parse-add-class rule-parse obj))))
         (field (fld-form-field fe))
         (args  (fld-form-args fe))
         ((pattern fp-val-var) (field-pattern type field args))
         (true-pattern  (if (and fp-val-var val-var)
                           (subst val-var fp-val-var pattern)
                          pattern)))
    (rule-parse-read-value-pattern rule-parse true-pattern (or val-var (gentemp "?O")))))
