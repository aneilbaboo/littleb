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

(in-package mallavar-utility)

(defun compute-form-type (form env &optional allowp)
  "Determines the type of the object computed by OBJ-FORM, or returns NIL."
  (let ((e-form     (macroexpand form env)))
    (cond
     ((the-expr-p e-form)
      (the-expr-type e-form))

     ((and allowp (allow-expr-p e-form))
      (allow-expr-type e-form))
     
     ((quoted-p form)
      (type-of (second form)))

     ((constantp e-form)    
      (type-of e-form)))))

(defun compute-form-class (form env &optional allowp)
  "Determines the class returned by form, or NIL if it is not possible to determine."
  (find-class (compute-form-type form env allowp) nil env))

(defun compile-time-computable-form-p (form)
  (or (quoted-p form)
      (constantp form)))

(defun compute-form-value (form env &optional error)
  "Attempts to determine the value of form at compile time.  
This is possible for constantp objects and quoted expressions.   
Returns: VALUE, T/NIL indicating whether it was possible 
to determine the value."
  (let ((f (macroexpand form env)))
    (cond
     ((quoted-p f)            (values (second f) t))
     ((constantp f)           (values (constant-value f) t))
     ((the-expr-p f)          (compute-form-value (the-expr-form f) env error))
     (error                   (error "~{~S.  ~}Cannot compute value of ~S at compile time"
                                     (if (stringp error) (list error))
                                     form))
     (t                       (values nil nil)))))

(defun constant-value (o)
  (assert (constantp o) ()
    "Invalid argument: ~S is not a constant." o)
  (cond 
   ((symbolp o)    (symbol-value o))
   ((the-expr-p o) (constant-value (the-expr-form o)))
   (t              o)))

(defun the-expr-p (o)
  (and (consp o)
       (eq (first o) 'the)))

(deftype the-expression ()
  `(satisfies the-expr-p))

(defun the-expr-type (o)
  (check-type o the-expression)
  (second o))

(defun the-expr-form (o)
  (check-type o the-expression)
  (third o))

(defun the-expr-reduce-to-form (e)
  "Reveals the form decorated by one or more THE operators (or returns the form if it is undecorated)."
  (if (the-expr-p e) (the-expr-reduce-to-form (the-expr-form e))
    e)) 

(defmacro allow (type form)
  `(the (or null ,type) ,form))

(deftype allow (type)
  `(or null ,type))

(defun allow-type-p (o)
  (and (consp o)
       (eq (first o) 'allow)
       (eq (length o) 2)))

(defun allow-type-type (o)
  (second o))

(defun allow-expr-p (o)
  (and (consp o)
       (eq (first o) 'allow)
       (eq (length o) 3)))

(defun allow-expr-type (o)
  (second o))

(defun allow-expr-form (o)
  (third o))
