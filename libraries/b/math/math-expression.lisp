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

;;; File: math-expressions
;;; Description: defines sum-expressions and product-expressions


(in-package b/math)

(include @folder/math-concept)

(defcon math-expression (:abstract math-concept) ())

;;;;
;;;; HELPER FUNCTIONS: 
;;;;        Used by sum-expression and product-expression 
;;;;

;;;
;;; var-value-list:  ((var1 . val1) (var1 . val1)...)
;;;        - a list of cons pairs, called ELEMENTS, sorted on the arbitrary index of the car
;;;        - elements may be added to the list using combine-var-value-element
;;;        - lists may be combined by combine-var-value-lists
;;;
(defun var-value-list (&rest args)
  (sort-var-value-list (map-plist (lambda (v c) (cons v c)) args)))

(defun sort-var-value-list (lst &optional  (copy t))
  (sort-by-arbitrary-index (if copy (copy-list lst) lst) :key #'car))

(defun combine-var-value-lists (l1 l2 &optional (op #'+op))
  (let ((new (copy-list l1)))
    (reduce (lambda (lc-elts elt)
              (combine-var-value-element lc-elts elt op nil))
            (cons new l2))))

(defun combine-var-value-element (lc-elts elt &optional (op #'+op) (copy t))
  (let ((elt-index (arbitrary-index (car elt)))
        (lc-elts   (if copy (copy-list lc-elts) lc-elts)))
    (labels ((try-add-to-lc-elts (rest)
               (cond 
                (rest
                 (let* ((next-elt (car rest))
                        (index (arbitrary-index (car next-elt))))
                   (cond ((eq index elt-index)
                          (cons (cons (car next-elt) (funcall op (cdr elt) (cdr next-elt)))
                                (cdr rest)))
                         ((< elt-index index)
                          (cons elt rest))
                         (t (setf (cdr rest) (try-add-to-lc-elts (cdr rest)))
                            rest))))
                (t (list elt)))))
      (try-add-to-lc-elts lc-elts))))

(defun call-op-subst (op substitutions &rest args)
  (let ((subst-op (getf substitutions op op)))
    (apply (or (operator subst-op) subst-op) args)))

