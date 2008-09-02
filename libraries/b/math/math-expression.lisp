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

