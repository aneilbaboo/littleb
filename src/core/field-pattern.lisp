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

;;; File: field-pattern
;;; Description: enables definition of field-pattern expressions (for use in rules)

;;; $Id: field-pattern.lisp,v 1.3 2008/09/02 14:58:10 amallavarapu Exp $
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
