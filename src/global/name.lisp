;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2003-2008 Aneil Mallavarapu

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


;;; File: name
;;; Description: defines the NAME class

;;; $Name:  $
;;;
;;;
(in-package b)


(defmacro setf-name (&body args &environment env)
  "Similar in usage to SETF: takes pairs of places & arguments which will be assigned to
those places.  The differences is that the arguments are evaluated in the context of an
NAME generated from the place.  
E.g.,  (setf-name P F) is syntactic sugar for (let ((Z (has-name P F)))
                                             (setf P Z))"
                                                  
  (flet ((compute-place-expansion (place)  (if (symbolp place) (macroexpand-1 place env) place)))
    (assert (evenp (length args)) () "setf-name requires an even number of arguments")

    `(progn 
       ,@(loop for iter = args then (cddr iter)
               for place = (first iter)
               for place-expansion = (compute-place-expansion place)
               for form = (second iter)
               while    iter               
               collect   `(setf ,place (has-name ,(if (symbolp place) place-expansion place)
                           ,form))))))
(port:setup-indent 'setf-name 0 7 7)

(defun make-name (o) 
  (typecase o
    (atom `(quote ,o))
    (cons (make-cons-name o))))

(defun make-cons-name (o)
  (let ((op (first o)))
    (when (or (not (symbolp op))
              (special-operator-p op)
              (macro-function op)
                      (not (fboundp op))) 
      (error "Attempt to make name from ~S, but operator ~S is undefined." 
             o (first o)))
    (case op
      (funcall `(quoted-name ',(second op) ,@(rest o)))
      (apply   `(apply #'quoted-name ',(second op) ,@(rest o)))
      (t       `(quoted-name ',op ,@(rest o))))))
    
(defun quoted-name (op &rest args)
  (assert (symbolp op))
  (cond
   ((and (eq op 'fld)
         (eq (second args) :apply)) 
    (apply #'list* 'fld (first args) (cddr args)))
   ((eq op 'apply) (apply #'list* args))
   (t             (list* op args))))

 


 (defmacro has-name (name rhs &environment env)
  "Defines evaluates rhs in the context of an name, which is generated from NAME.  
The name will be assigned as the object-name of the value returned by rhs. 
Returns the value of rhs."
  (let ((val              (gensym "VAL"))
        (name             (cond
                           ((local-field-symbol-p name) (macroexpand-1 name env))
                           ((local-field-symbol-form-p name) `(fld object
                                                                   ,(local-field-symbol-to-keyword (first name))
                                                                   ,@(rest name)))
                           (t name))))
                                
    `(let* ((*NAME*     ,(make-name name))
            (,val     ,rhs))
       (unless (gethash ,val +NAMES+)
         (setf (object-name ,val) *NAME*))
       ,val)))

;;;; (defun make-name (head &optional (field ?) &rest args)
;;;;   (let+ (((true-field true-args) (if (eq field :apply)
;;;;                                      (values (first args) (fix-apply-args (rest args)))
;;;;                                    (values field args)))
;;;;          (new        (_make-name head true-field true-args))
;;;;          (existing   (gethash new +NAMES+)))
;;;;     (ifit existing it
;;;;       (setf (gethash new +NAMES+) new))))

(defun apply-arglist-to-funcall-arglist (args)
  "Converts a list representing arguments to an apply call to a list representing the equivalent funcall call."
  (case (length args)
    (1         (first args))
    (otherwise (nconc (butlast args) (last args)))))
          

;; get the name associated with an object
(defun object-name (obj)
  (let ((name (gethash obj +NAMES+)))
    (and (eq (dereference-name name) obj)  
         name)))
    
(defun dereference-name (name)
 (ignore-errors (eval name)))


(defun (setf object-name) (name object)
  (setf (gethash object +NAMES+) name))


(defmethod fld :around (o (field (eql :_name)) &rest args)
  (declare (ignore field args))
  (object-name o))

(defmethod (setf fld) :around (name o (field (eql :_name)) &rest args)
  (declare (ignore field args))
  (setf (object-name o) name))




;;;
;;; A mess.   The print system here and elsewhere needs to be rewritten.
;;;           The idea is to provide evaluation-identity print semantics.
;;;           This means that the printed form of an object is equivalent
;;;           to a string which could be evaluated by the little b interpreter
;;;           to produce the same (EQ) object. 
;;;; ;;;
;;;; (defmethod print-object ((i name) stream)
;;;; ;  (print-unreadable-object (stream i :type t :identity t)))
;;;;   (with-print-context i
;;;;     (princ "{_name " stream)
;;;;     (print-name i stream)
;;;;     (princ #\} stream)))


(defun print-name-obj (o stream)
  (typecase o
    (symbol (print-symbol-name o stream))
    (name     (print-name o stream))
    (t      (prin1 o stream))))
         

(defun print-symbol-name (o stream)
  (let ((pkg          (symbol-package o))
        (name         (symbol-name o)))
    (cond
     ((null pkg) (princ "#:" stream))

     (t 
      (let ((inpkg-symbol (find-symbol name *package*)))
        (unless (eq inpkg-symbol o)
          (format stream "~A:" (string-downcase (package-name pkg)))
          (unless (symbol-external-p o)
            (princ #\: stream))))))
    (princ (string-downcase name) stream)))

(defsetf object (&rest args) (x)
  (declare (ignore x))
  (b-error "Cannot assign value to ~A.  An object is not a valid SETF place."
           `(object ,@args)))
  
(defun export-field-symbols (o)
  (when (and (symbolp *NAME*)
             (symbol-external-p *NAME*))
    (let ((spackage (symbol-package *NAME*)))
      (mapcar (lambda (f)
                (export 
                 (intern (format nil "~S.~A" *NAME*  f)
                         spackage)
                 spackage))
              (fld o :_fields)))))

