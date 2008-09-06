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


;;;
;;;  The MTRIE Package:
;;;  Contains functions for defining a trie, a tree which stores
;;;
;;;

;;;;
;;;; EXAMPLE:
;;;;
;;;; (defun string-descendent-p (x y)
;;;;   (let ((sg (string-greaterp x y)))
;;;;     (and sg (> sg 0))))

;;;; 
;;;; (setf dt (make-trie #'string-descendent-p #'string-equal))
;;;; (find-trie-node "a" dt t)
;;;; (find-trie-node "abc" dt t)
;;;; (find-trie-node "b" dt t)
;;;; (find-trie-node "bcd" dt t)
;;;; (find-trie-node "c" dt t)
;;;; (find-trie-node "cde" dt t)
;;;; (find-trie-node "cd" dt t)    
;;;; 
(defpackage :mallavar-trie
  (:nicknames :mtrie)
  (:export #:trie #:trie-node #:print-trie #:make-trie #:make-test-trie
           #:find-trie-node #:trie-descendents
           #:remove-trie-node #:trie-ancestors
                   
           #:trie-node-key #:trie-node-value #:trie-node-children

           #:trie-root-node #:trie-descendent-predicate #:trie-equality-predicate))

(in-package :mallavar-trie)

(defstruct (trie (:constructor make-trie (descendent-predicate 
                                            &optional
                                            (equality-predicate #'equalp))))
  (descendent-predicate nil :read-only t)
  (equality-predicate nil :read-only t)
  (root-node (make-trie-node nil nil nil) :read-only t))

(defmethod print-object ((o trie) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (if *print-pretty*
      (print-trie o stream))))

(defstruct (trie-node (:constructor make-trie-node (key children value)))
  (key nil :read-only t)
  children
  value)

(defmethod print-object ((o trie-node) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "KEY: ~S; VALUE: ~S; CHILDREN: ~S" 
            (trie-node-key o)
            (trie-node-value o)
            (length (trie-node-children o)))))


(defun trie-descendents (x)
  (etypecase x
    (trie      (trie-descendents (trie-root-node x)))
    (trie-node (let ((c (trie-node-children x)))
                 (append c
                         (mapcan #'trie-descendents c))))))

(defun remove-trie-node (key trie)
  (loop with parent = (find-trie-parent-node key trie)
        with eq-pred = (trie-equality-predicate trie)
        with children = (trie-node-children parent)
        for last = nil then rest
        for rest on children
        for c = (first rest)
        when (funcall eq-pred (trie-node-key c) key)
        do (if last (setf (rest last) (rest rest))
             (setf (trie-node-children parent) (rest rest)))
           (return c)))
          
(defun find-trie-node (key trie &optional (createp nil))
  (cond
   (key (let* ((ancestors (trie-ancestors key trie))
               (parent    (or (first (last ancestors))
                              (trie-root-node trie)))
               (equal-p   (trie-equality-predicate trie))
               (node      (or (find key (trie-node-children parent) :test equal-p :key #'trie-node-key)
                              (when createp
                                (insert-trie-node (make-trie-node key nil nil) parent trie)))))
          (values node ancestors)))
   (t   (values (trie-root-node trie) nil))))

(defun find-trie-parent-node (key trie)
  (first (last (trie-ancestors key trie))))

(defun trie-ancestors (key trie)
  (let ((pred (trie-descendent-predicate trie)))
    (labels ((descendent-p (x y) (or (null y) (funcall pred x y)))
             (find-node (node)
               (when node
                 (cons node
                       (find-node 
                        (find-if (lambda (cnode) (descendent-p key (trie-node-key cnode)))
                                 (trie-node-children node)))))))
      (when key 
        (find-node (trie-root-node trie))))))

(defun insert-trie-node (node parent trie)
  (loop with key = (trie-node-key node)
        with descendent-p = (trie-descendent-predicate trie)
        for c in (trie-node-children parent)
        if (funcall descendent-p (trie-node-key c) key)
        collect c into node-children
        else
        collect c into parent-children
        finally (setf (trie-node-children node) node-children
                  (trie-node-children parent) (if (eq node (trie-root-node trie)) parent-children
                                                (cons node parent-children)))
                (return node)))


(defun print-trie (x &optional (stream *standard-output*))
  (etypecase x
    (trie-node (pprint-logical-block (stream nil)
                  (princ "+ "stream)
                  (pprint-logical-block (stream nil)
                    (format stream "KEY: ~:[ROOT~;~S~]" (trie-node-key x) (trie-node-key x))
                    (pprint-newline :mandatory stream)
                    (format stream "VALUE: ~S" (trie-node-value x))
                    (dolist (c (trie-node-children x))                      
                      (pprint-newline :mandatory stream)
                      (print-trie c stream)))))
    (trie      (print-trie (trie-root-node x) stream))))

