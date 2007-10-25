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

;;; File: operators
;;; Description: 

;;; $Id: operators.lisp,v 1.2 2007/10/25 14:44:23 amallavarapu Exp $
;;;
(in-package b/math)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +op-precedence-expt+  200)
(defconstant +op-precedence-muldiv+  400)
(defconstant +op-precedence-addsub+ 500)
(defconstant +op-precedence-eq-test+ 700)
(defconstant +op-precedence-equivalence+ 800)
(defconstant +op-precedence-assignment+ 1200)
(defconstant +op-precedence-function+ 100)

;; xfx = non associative ; not implemented
;; yfx = left associative  
;; xfy = right associative

(add-operator '_name +op-precedence-function+ :fx '_name)
(add-operator '+ +op-precedence-addsub+ :yfx '+op)
(add-operator '- +op-precedence-addsub+ :yfx '-op)
(add-operator '* +op-precedence-muldiv+ :yfx '*op)
(add-operator '/ +op-precedence-muldiv+ :yfx '/op)
(add-operator '^ +op-precedence-expt+ :xfy '^op)
(add-operator '= +op-precedence-eq-test+ :xfy '=op)
(add-operator '== +op-precedence-equivalence+ :xfy '==op)
(add-operator ':+= +op-precedence-equivalence+ :xfy '==+op)
(add-operator '==- +op-precedence-equivalence+ :xfy '==-op)
(add-operator := +op-precedence-assignment+ :xfy 'setf)
(add-operator :#= +op-precedence-assignment+ :xfy 'setf-name)
(add-operator :# (1- +op-precedence-assignment+) :xfy 'has-name)
(add-operator '> +op-precedence-eq-test+ :xfy '>op)
(add-operator '>= +op-precedence-eq-test+ :xfy '>=op)
(add-operator '< +op-precedence-eq-test+ :xfy '<op)
(add-operator '<= +op-precedence-eq-test+ :xfy '<=op)
 
(defgeneric +op (n p))
(defgeneric -op (lhs rhs))
(defgeneric /op (lhs rhs))
(defgeneric *op (n p))
(defgeneric ^op (n p))
(defgeneric =op (lhs rhs))
(defgeneric ==op (lhs rhs))
(defgeneric ==-op (lhs rhs))
(defgeneric ==+op (lhs rhs))

;;;
;;; external functions for arithmetic
;;; \
(defun mreduce (op args)
  (if (> (length args) 1) (reduce op args)
    {(first args)}))

(defun s+ (&rest args)
  (when args (mreduce #'+op args)))

(defun s* (&rest args)
  (when args (mreduce #'*op args)))

(defun s/ (&rest args)
  (when args (mreduce #'/op args)))

(defun s- (&rest args)
  (when args (mreduce #'-op args)))

(defun s^ (&rest args)
  (when args
    (mreduce
     (lambda (pow obj)
       (^op obj pow))
     (reverse args))))
                         
(defun s= (&rest args)
  (when args (mreduce #'op= args)))

(defun s> (&rest args)
  (when args (mreduce #'op> args)))

(defun s>= (&rest args)
  (when args (mreduce #'op>= args)))

(defun s< (&rest args)
  (when args (mreduce #'op< args)))

(defun s<= (&rest args)
  (when args (mreduce #'op<= args)))


(defun arithmetic-method-undefined (op x1 x2)
  (b-error "{~S ~S ~S} undefined:  no operator method for ~S ~S ~S." x1 op x2 (type-of x1) op (type-of x2)))

;;;
;;; +OP
;;;
(defmethod +op ((lhs null) rhs)  (as-math-arg rhs))

(def-commutative-operator-method +op :around ((n null) o) (as-math-arg o))

(defmethod +op ((s1 sum-expression) (s2 sum-expression))
  (sum-expression (combine-var-value-lists (|SUM-EXPRESSION.VARIABLE| s1)
                                           (|SUM-EXPRESSION.VARIABLE| s2))
                  (+op  (|SUM-EXPRESSION.NUMERIC| s1)
                        (|SUM-EXPRESSION.NUMERIC| s2))))

(defmethod +op ((n1 number) (n2 number))
  (cl:+ n1 n2))

(def-commutative-operator-method +op ((n1 number numeric-concept) (n2 numeric-concept))
  (arithmetic-method-undefined '+ n1 n2))

(def-commutative-operator-method +op ((s sum-expression) (n number numeric-concept))
  [sum-expression (|SUM-EXPRESSION.VARIABLE| s)
                  (+op (|SUM-EXPRESSION.NUMERIC| s) n)])

(def-commutative-operator-method +op ((s sum-expression) o)
  (sum-expression (combine-var-value-element (|SUM-EXPRESSION.VARIABLE| s)
                                             (sum-element o 1))
                  (|SUM-EXPRESSION.NUMERIC| s)))

(def-commutative-operator-method +op ((p1 product-expression) (p2 product-expression))
  (sum-expression (combine-var-value-element (list (sum-element p1)) 
                                             (sum-element p2))))

(defmethod +op (o1 o2)
  (arbitrary-index o1) ; a little hack suggest o1 appear before o2
  (sum-expression (combine-var-value-element (list (sum-element o1)) (sum-element o2))))

(def-commutative-operator-method +op (o (n number numeric-concept))
  (sum-expression (list (sum-element o)) n))

(defmethod +op ((q1 quantity) (q2 quantity))
  (quantity (+op (|QUANTITY.BASE-MAGNITUDE| q1)
                 (|QUANTITY.BASE-MAGNITUDE| q2))
            (|QUANTITY.DIMENSION| q1)))

(defmethod +op ((d1 dimension) (d2 dimension))
  (unless (or (null d1) (null d2) (eq d1 d2))
    (b-error "Cannot combine dimensions ~S + ~S" d1 d2))
  d1)

;;;
;;; *OP
;;;
(def-commutative-operator-method *op :around ((n null) o) (as-math-arg o))
  
(defmethod *op ((n1 number) (n2 number))  (cl:* n1 n2))

(def-commutative-operator-method *op ((m math-concept) o)
  (arithmetic-method-undefined '* m o))

(def-commutative-operator-method *op ((m math-expression) o)
  (*op (product-expression (list (product-element m 1))) o))

;;;; (def-commutative-operator-method *op ((n1 numeric-concept) (n2 numeric-concept number))
;;;;   (arithmetic-method-undefined '* n1 n2))

(def-commutative-operator-method *op (o (n number numeric-concept))
  (multiplicity-sum-expression o n))

(defmethod *op ((p1 product-expression) (p2 product-expression))
  (product-expression (combine-var-value-lists (the product-expression p1).variable
                                               (the product-expression p2).variable)))

(def-commutative-operator-method *op ((p product-expression) (s sum-expression t))
  (cond
   ((multiplicity-sum-expression-p s)
    (with-multiplicity-sum-expression s (var coef)
      (*op (*op p var) coef)))

   (t (product-expression (combine-var-value-element (the product-expression p).variable
                                                     (product-element s 1))))))

(def-commutative-operator-method *op ((u unit) x)
  (*op (quantity 1 u) x))

(def-commutative-operator-method *op ((s sum-expression) (n number numeric-concept unit))
  (cond
   ((equalp n 1) s)
   ((zero-numeric-p n) (quantity 0 (*op (dimension-of n) (dimension-of s))))

   ((multiplicity-sum-expression-p s) 
    (with-multiplicity-sum-expression s (var coef)
      (multiplicity-sum-expression var (*op coef n))))
   ((equalp n -1) (*op-expand s n))
  
   (t (call-next-method))))

(def-commutative-operator-method *op ((s sum-expression) o)
  (cond
   ((multiplicity-sum-expression-p s)
    (with-multiplicity-sum-expression s (var coef)
      (if (product-expression-p var) (*op (*op var o) coef)
        (let ((prod (product-expression (combine-var-value-element (list (product-element var))
                                                                   (product-element o)))))
          (*op prod coef)))))

   (t (product-expression (combine-var-value-element (list (product-element s))
                                                     (product-element o))))))
                                    
(def-commutative-operator-method *op ((p product-expression) (n number numeric-concept))
  (cond 
   ((equalp n 1) p)
   ((zero-numeric-p n) (quantity 0 (*op (dimension-of p) (dimension-of n))))
   (t (multiplicity-sum-expression p n))))

(defmethod *op ((s1 sum-expression) (s2 sum-expression))
  (cond
   ((and (unit-sum-expression-p s1)
         (unit-sum-expression-p s2)) (product-expression (combine-var-value-lists 
                                                            (the sum-expression s1).variable
                                                            (the sum-expression s2).variable)))

   ((and (multiplicity-sum-expression-p s1)
         (multiplicity-sum-expression-p s2)) 
    (with-multiplicity-sum-expression s1 (s1-var s1-coef)
      (with-multiplicity-sum-expression s2 (s2-var s2-coef)
        (*op (*op s1-var s2-var) (*op s1-coef s2-coef)))))

   (t (product-expression (combine-var-value-element (list (product-element s1))
                                                     (product-element s2))))))
    

(defmethod *op (o1 o2)
  (arbitrary-index o1) ; a hack to suggest that o1 appear before o2
  (product-expression (combine-var-value-element (list (product-element o1))
                                                 (product-element o2))))
           

(def-commutative-operator-method *op ((x number) (u unit))
  (quantity (convert-to-unit x u t)
            (dimension-of u)))

(defmethod *op ((u1 unit) (u2 unit))
  (let ((c1 u1.conversion)
        (c2 u2.conversion)
        (d  (*op (the dimension u1.dimension)
                 (the dimension u2.dimension))))
    (assert (and (realp c1) (realp c2)) ()
      "Cannot multiply ~S and ~S - both units must be scalable." u1 u2)
    
    (cond ((dimension-p d) [unit d (* c1 c2)])
          (t               (* c1 c2)))))

(defmethod *op ((d1 dimension) (d2 dimension))
  (combine-dimensions d1 d2))

(def-commutative-operator-method *op ((d dimension) (n (eql nil))) nil)

(def-commutative-operator-method *op ((nd (eql null-dimension)) (o t))
    o)

(def-commutative-operator-method *op ((d dimension) (n number))
  (cond
   ((or (equalp n 1)
        (equalp n 0))   d)
   (t                   (arithmetic-method-undefined '* d n))))

(def-commutative-operator-method *op ((q quantity) (n number))
  (quantity (*op (|QUANTITY.BASE-MAGNITUDE| q) n)
            (|QUANTITY.DIMENSION| q)))

(def-commutative-operator-method *op ((q1 quantity) (q2 quantity))
  (quantity (*op (|QUANTITY.BASE-MAGNITUDE| q1) (|QUANTITY.BASE-MAGNITUDE| q2))
            (*op (|QUANTITY.DIMENSION| q1) (|QUANTITY.DIMENSION| q2))))

(def-commutative-operator-method *op ((q quantity) (u unit))
  (*op (the quantity q) (the quantity (quantity 1 u))))

;;;
;;; ^OP
;;;
(defmethod ^op :around (o (p (eql 0)))   1)
(defmethod ^op :around (o (p (eql 0.0))) 1)  
;(defmethod ^op :around (o (p float))      (call-next-method o (rationalize p)))

(defmethod ^op ((e null) p) nil)
(defmethod ^op ((n1 number) (n2 number)) 
  (careful-expt n1 n2)) ; explored the idea of returning a product-expression

(defmethod ^op ((s sum-expression) (p number))
  (cond 
   ((multiplicity-sum-expression-p s)
    (with-multiplicity-sum-expression s (var coef)
      (*op (^op var p) (^op coef p))))

   (t (product-expression (list (product-element s p))))))

(defmethod ^op ((n number) (p math-expression))
  (product-expression (list (product-element n p))))

(defmethod ^op ((n numeric-concept) (p math-expression))
  (product-expression (list (product-element n p))))

(defmethod ^op (m p)
  (product-expression (list (product-element m p))))

(defmethod ^op ((p product-expression) o)
  (product-expression (mapcar (lambda (pe)
                                (product-element (product-element-var pe)
                                                 (*op (product-element-power pe) o)))
                                             (the product-expression p).variable)))

(defmethod ^op ((u unit) (p real))
  (let ((c u.conversion))
    (cond 
     ((realp c)
      [unit (^op (the dimension u.dimension) p) (careful-expt c p)])
     (t (b-error "Cannot perform exponentiation on ~S - not a scalable unit." u)))))

(defmethod ^op ((q quantity) (p number))
  (quantity (^op (|QUANTITY.BASE-MAGNITUDE| q) p)
            (^op (|QUANTITY.DIMENSION| q) p)))

(defmethod ^op ((n (eql null-dimension)) p)
  null-dimension)

(defmethod ^op ((d dimension) p)
  (cond
   ((eq d null-dimension) null-dimension)
   ((eq p 0) null-dimension)
   (t (exponentiate-dimension d p))))

(defun exponentiate-dimension (d p)
  (etypecase d
   (base-dimension
      (make-dimension-from-dimension-power-list `((,d . ,p))))
   (derived-dimension
    (make-dimension-from-dimension-power-list
     (mapcar (lambda (dp)
               (cons (dimension-element-base dp)
                     (*op (dimension-element-power dp) p)))
             d.base-dimensions)))))
;;;
;;; -OP
;;;
(defmethod -op (x1 x2)
  (+op (or x1 (quantity 0 (dimension-of x2))) 
       (*op (or x2 (quantity 0 (dimension-of x1))) -1)))

;;;
;;; /OP
;;;
(defmethod /op (lhs rhs)
  (*op (or lhs 1) (^op rhs -1)))


;;;
;;; MISC other operators
;;;
(defconstant +nil-equality+ '(t))
(def-commutative-operator-method =op :around  ((n null) o) (if (or (null o)
                                                                   (eq o +nil-equality+))
                                                               +nil-equality+))
(def-commutative-operator-method >op :around ((n null) o) nil)
(def-commutative-operator-method >=op :around ((n null) o) nil)
(def-commutative-operator-method <op :around ((n null) o) nil)
(def-commutative-operator-method <=op :around ((n null) o) nil)

(defmethod =op (lhs rhs) (if (equalp lhs rhs) lhs))

(defmethod >op (n1 n2)
  (if (> n1 n2) n1))

(defmethod >=op (n1 n2)
  (if (>= n1 n2) n1))

(defmethod <op (n1 n2)
  (if (< n1 n2) n1))

(defmethod <=op (n1 n2)
  (if (<= n1 n2) n1))

(defmethod =op ((q1 quantity) (q2 quantity))
  (if (and (=op (|QUANTITY.DIMENSION| q1) (|QUANTITY.DIMENSION| q2))
           (=op (|QUANTITY.BASE-MAGNITUDE| q1) (|QUANTITY.BASE-MAGNITUDE| q2)))
      q1))


(defmethod >op ((q1 quantity) (q2 quantity))
  (check-dimensions q1 q2 '>)
  (if (> q1.base-magnitude q2.base-magnitude)
      q1))

(defmethod >=op ((q1 quantity) (q2 quantity))
  (check-dimensions q1 q2 '>=)
  (>= q1.base-magnitude q2.base-magnitude))

(defmethod <op ((q1 quantity) (q2 quantity))
  (check-dimensions q1 q2 '<)
  (< q1.base-magnitude q2.base-magnitude))

(defmethod <=op ((q1 quantity) (q2 quantity))
  (check-dimensions q1 q2 '<=)
  (<= q1.base-magnitude q2.base-magnitude))


)
