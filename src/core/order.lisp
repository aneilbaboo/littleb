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



;;; File: order
;;; Description: Provides a function, order, for uniquely ordering lists of objects.
;;;              Important for maintaining object identity when an ID field consists
;;;              of a list in which the order of elements is unimportant.
;;;              ORDER is typically used within an object constructor to ensure that
;;;              the list is in a canonical order.  

;;; $Id: order.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $

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

