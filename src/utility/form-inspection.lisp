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
