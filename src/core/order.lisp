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


;;; File: order
;;; Description: Provides a function, order, for uniquely ordering lists of objects.
;;;              Important for maintaining object identity when an ID field consists
;;;              of a list in which the order of elements is unimportant.
;;;              ORDER is typically used within an object constructor to ensure that
;;;              the list is in a canonical order.  

;;; $Id: order.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $

(in-package b)

(defconstant *arbitrary-order-table* (make-hash-table :test 'equalp :size 2003))
(defparameter *arbitrary-order-index* 0)

(defun order (&rest seq)
  (sort (copy-seq (if (eq (length seq) 1)
                      (car seq) seq))
        (lambda (o1 o2) 
          (< 0 (grouping-order o1 o2)))))

(defun sort-by-arbitrary-index (sequence &key (key #'identity) reverse)
  (sort sequence (if reverse #'> #'<) :key (lambda (o) (arbitrary-index (funcall key o)))))

(defun arbitrary-index (obj)
  (ifit (gethash obj *arbitrary-order-table*)
      it
    (setf (gethash obj *arbitrary-order-table*) 
          (incf *arbitrary-order-index*))))      

;; type-based order
(defmethod grouping-order (obj1 obj2)
  (let ((type-order (grouping-order (type-of obj1) (type-of obj2))))
    (if (zerop type-order)
        (grouping-order (arbitrary-index obj1) (arbitrary-index obj2))
      type-order)))

;; symbol order
(defmethod grouping-order ((obj1 symbol) (obj2 symbol))
  (if (eq obj1 obj2) 0
    (let* ((p1 (symbol-package obj1))
           (p2 (symbol-package obj2))
           (porder (grouping-order p1 p2)))
      (if (zerop porder)
          (let ((s1 (symbol-name obj1))
                (s2 (symbol-name obj2)))
            (grouping-order s1 s2))
        porder))))

;; string order
(defmethod grouping-order ((obj1 string) (obj2 string))
  (cond ((string= obj1 obj2) 0)
        ((string> obj1 obj2) 1)
        (t -1)))

;; number order
(defmethod grouping-order ((obj1 number) (obj2 number))
  (- obj1 obj2))

;; complex order
(defmethod grouping-order ((obj1 complex) (obj2 complex))
  (let ((real1 (realpart obj1))
        (real2 (realpart obj2)))
    (if (eq real1 real2)
        (- (imagpart obj1) (imagpart obj2))
      (- real1 real2))))

;; sequence order
(defmethod grouping-order ((obj1 sequence) (obj2 sequence))
  (let* ((len1 (length obj1))
         (len2 (length obj2))
         (len-dif (- len1 len2)))
    (if (zerop len-dif)
        (dotimes (index len1 0)
          (let ((elt-order (grouping-order (elt obj1 index) (elt obj2 index))))
            (if (not (zerop elt-order))
                (return elt-order))))
      len-dif)))
  

(defmethod grouping-order ((obj1 cons) (obj2 cons))
  (let ((car-order (grouping-order (car obj1) (car obj2))))
    (if (not (zerop car-order)) car-order
      (grouping-order (cdr obj1) (cdr obj2)))))

(defmethod grouping-order ((obj1 character) (obj2 character))
  (cond ((char= obj1 obj2) 0)
        ((char> obj1 obj2) 1)
        (t -1)))

