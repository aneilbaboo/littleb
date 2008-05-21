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
(in-package #I@FOLDER)
(include @FOLDER/dtype :use)

(defconstant +optypes+ '(:yfx :xfy :xf :fx :frest))
(defconstant +operator+ 'operator)
(defconstant +operator-precedence+ 'operator-precedence)
(defconstant +operator-type+ 'operator-type)
(defconstant +default-operator+ '*)

(defun add-operator (symbol precedence optype function)
  "This function is subject to change: may be replaced by a macro (e.g.,defoperator) in public interface."
  (assert (symbolp symbol) () "Expecting a symbol, but received ~S" symbol)
  (assert (integerp precedence) () "Expecting an integer, but received ~S" precedence)
  (assert (member optype +optypes+) () "Optype should be one of ~S" +optypes+)
  (if (not (keywordp symbol)) (export symbol))

  (setf (get symbol +operator+) function
        (get symbol +operator-precedence+) precedence
        (get symbol +operator-type+) optype)

  symbol)

(port:define-dspec-class defoperator () "")
(port:define-dspec-form-parser defoperator (name)
  `(defoperator ,name))
(port:define-dspec-form-parser def-macro-operator (name)
  `(defoperator ,name))

(defun operator-definition (symbol precedence optype lisp-op lambda-list body)
  (let ((prefix-name (intern (mkstr symbol "%OPERATOR") #I@folder)))
    `(port:dspec (defoperator ,symbol)
       (expose-symbol ',symbol)
       (,lisp-op ,prefix-name ,lambda-list ,@body)
       (add-operator ',symbol ,precedence ,optype ',prefix-name))))

(define-macro defoperator (symbol (precedence optype) lambda-list &body body)
  "Defines an infix operator with function evaluation semantics.  (DEFOPERATOR (SYMBOL PRECEDENCE OPTYPE) BODY)
Where BODY= lambda-list form* or a symbol denoting a function"
  (operator-definition symbol precedence optype 'defun lambda-list body))

(define-macro def-macro-operator (symbol (precedence optype) lambda-list &body body)
  "Defines an infix operator with macro evaluation semantics.  (DEFOPERATOR (SYMBOL PRECEDENCE OPTYPE) BODY)
Where BODY= lambda-list form* or a symbol denoting a macro"
  (operator-definition symbol precedence optype 'defmacro lambda-list body))

(defun delete-operator (symbol)
  (setf (get symbol +operator+) nil
        (get symbol +operator-precedence+) nil
        (get symbol +operator-type+) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun operator (s)
  "Returns the operator function is s is an operator, NIL otherwise"
  (and (symbolp s)
       (get s +operator+))))

(defun operator-precedence (s)
  "Given a symbol s, returns the precedence (or NIL if s is not an operator)"
  (get s +operator-precedence+))

(defun operator-type (s)
  (get s +operator-type+))

(defparameter *math-top-level* t)

(defmacro ensure-math-arg (&body body)
  (let ((ret (gensym "RET")))
    `(let* ((*math-top-level* nil)
            (,ret (progn ,@body)))
       (if *math-top-level* (as-math-arg ,ret) ,ret))))


;;;; (defun make-math-expression-loader (o)
;;;;   (include-dynamic 'b/math)
;;;;   (let ((fun (symbol-function (find-symbol "MAKE-MATH-EXPRESSION"
;;;;                                            'b/math))))
;;;;     (setf *make-math-expression* fun)
;;;;     (funcall fun o)))

;;;; (defparameter *make-math-expression* #'make-math-expression-loader)

#-clisp 
(defun as-math-arg (o)
  (typecase o 
    (numeric            o)
    (math-expression    o)
    (null               nil)
    (t                  (make-math-expression o))))

#+clisp ; clisp's typecase macro compares classes and requires the classes referenced by the types to be defined.
        ; since numeric, math-expression, null are not yet defined, this fails
        ; here is a COND version:
(defun as-math-arg (o)
  (cond
   ((typep o 'numeric)         o)
   ((typep o 'math-expression) o)
   ((typep o 'null)            nil)
   (t                          (make-math-expression o))))

(defmacro math (&rest expr)
  (cond
   ;; convert to a valid mathematical object (numeric or math-expression)
   ((= (length expr) 1)   `(as-math-arg ,(first expr)))
    
   ;; otherwise, infix code is computed & evaluated:
   (t                     (compute-prefix-form (apply-default-operator expr) #'operator))))


(defun find-highest-operator-pos (seq)
  (let ((highest-op (reduce #'higher-precedence-operator seq)))
    (when highest-op 
      (case (operator-type highest-op)
        ((:xfy :xf)  (position highest-op seq :from-end nil))
        (t           (position highest-op seq :from-end t))))))
        
(defun higher-precedence-operator (o1 o2)
  (let ((o1-op? (operator o1))
        (o2-op? (operator o2)))
    (cond        
     ((and o1-op? (not o2-op?))    o1)
     ((and o2-op? (not o1-op?))    o2)
     ((not (or o1-op? o2-op?))     nil)
     ((> (operator-precedence o1) 
         (operator-precedence o2)) o1)
     (t                             o2))))

(defun compute-prefix-form (expr &optional (prefix-operator #'identity))
  "Converts the expression to prefix form"
  (if (< (length expr) 2) (first expr)
    (let ((high-op-pos  (find-highest-operator-pos expr))) 
      (cond
       (high-op-pos (infix-op-code (elt expr high-op-pos)
                                   (let ((*math-top-level* nil)) (subseq expr 0 high-op-pos))
                                   (let ((*math-top-level* nil)) (subseq expr (1+ high-op-pos)))
                                   prefix-operator))
       (t (infix-op-code +default-operator+ (subseq expr 0 1) (subseq expr 1)
                         prefix-operator))))))
    
(defun apply-default-operator (expr)
  (flet ((get-state (arg)
           (let ((optype (and (symbolp arg)
                              (operator-type arg))))
             (case optype
               ((:xf :yfx :xfy :fx :frest) optype)
               (t :arg)))))
    (let* ((1st   (first expr))
           (state (get-state 1st))
           (ret   (list 1st)))
      (loop with iter = (cdr expr)
            while iter
            do (let* ((arg (first iter))
                      (next-state (get-state arg)))
                 (case state
                   ((:arg :xf)       (if (not (member next-state '(:xf :yfx :xfy)))
                                         (push +default-operator+ ret))
                                     (push arg ret)
                                     (setf iter (rest iter)))
                                     (setf iter (rest iter))
                   ((:yfx :fx :xfy)  (push arg ret)
                                     (setf iter (rest iter)))
                   (:frest      (setf ret (nconc (reverse iter) ret))
                                (setf iter nil)))
                 (setf state next-state)))
      (reverse ret))))

(defun infix-op-code (op lhs rhs &optional (prefix-operator #'operator))
  (labels ((args-present-error (side)
             (error "Unexpected ~A arguments for operator ~S." side op))
           (lhs-present-error () (args-present-error "left hand side"))
           (make-op-call (opfn splice &rest args)
            (if splice
              (if (symbolp (first opfn))
                  `(,@opfn ,@args)
                `(funcall ',(first opfn) ,@(rest opfn) ,@args))
              (if (symbolp opfn)
                  `(,opfn ,@args)
                `(funcall ,opfn ,@args)))))
    (multiple-value-bind (prefix-op splice) (funcall prefix-operator op)
      (assert prefix-op () "No operator found in math-form")
      (case (operator-type op)
        ;; infix
        ((:yfx :xfy) 
         (make-op-call prefix-op splice (compute-prefix-form lhs prefix-operator)
                       (compute-prefix-form rhs prefix-operator)))
        ;; unary postfix
        (:xf   
         (when rhs (args-present-error "right hand side"))
         (make-op-call prefix-op splice (compute-prefix-form lhs prefix-operator)))
        
        ;; unary prefix
        (:fx
         (when lhs (lhs-present-error))
         (make-op-call prefix-op splice (compute-prefix-form rhs prefix-operator)))

        ;; function
        (:frest
         (when lhs (lhs-present-error))
         (make-op-call prefix-op splice rhs))))))



(set-macro-character #\{ #'b::curly-brace-reader nil *working-readtable*)
