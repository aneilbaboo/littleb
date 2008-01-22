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

;;; File: dictionary
;;; Description: the special dictionary object.  
;;;              Provides an object that has fields which are flexibly added.
;;;

;;; $Id: dictionary.lisp,v 1.7 2008/01/22 16:42:40 amallavarapu Exp $
(in-package b/util)

(include-declaration :expose-packages (mallavar-utility)
                     :expose-symbols (do-dictionary))

(defconstant +dictionary-default-test+ 'equalp)
(defconstant +dictionary-default-rehash-size+ 2.0)
(defconstant +dictionary-default-rehash-threshold+ .75)
(defconstant +dictionary-default-size+ 10)

(defcon dictionary (:notrace) 
  (&optional (id := *name*)
   &property (_hash-table := (make-hash-table :test +dictionary-default-test+
                                              :rehash-size +dictionary-default-rehash-size+ 
                                              :rehash-threshold +dictionary-default-rehash-threshold+
                                              :size +dictionary-default-size+))))

(defmethod fld ((object hash-table) (field (eql :_list)) &rest args)
  (declare (ignore field args))
  (loop for v being the hash-values of object
        collect v))

(defmethod fld ((object hash-table) (field (eql :_map)) &rest args)
  (declare (ignore field))
  (loop with fn = (first args)
        for v being the hash-values of object
        collect (funcall fn v)))

(define-macro do-dictionary ((vars dictionary &optional collect-results) &body body)
  (let+ (((valvar keyvar) (if (consp vars) (values-list vars) vars))
         (dict    (gensym "DICTIONARY"))
         (result (gensym "RESULT")))
     `(loop with ,dict = ,dictionary
               for ,valvar being the hash-values of (|DICTIONARY._HASH-TABLE| ,dict)
               ,@(when keyvar `(for ,keyvar being the hash-keys of ,dict))
               for ,result = (progn ,@body)
               ,@(when collect-results `(collect ,result)))))

(defield dictionary._set-params (&rest args 
                                   &key (test +dictionary-default-test+)
                                        (rehash-size +dictionary-default-rehash-size+)
                                        (rehash-threshold +dictionary-default-rehash-threshold+)
                                        (size +dictionary-default-size+))
  (declare (ignorable test rehash-size rehash-threshold size))
  (let ((ht     ._hash-table)
        (new-ht (apply #'make-hash-table args)))
    (if ht (loop for k being the hash-keys of ht
                 for v being the hash-values of ht
                 do (setf (gethash k new-ht) v)))
    (setf (|DICTIONARY._HASH-TABLE| object) new-ht)))

(defield dictionary._length () (hash-table-count ._hash-table))

(defield dictionary._map (fn)
    (loop with ht = ( |DICTIONARY._HASH-TABLE| object)
        for v being the hash-values of ht
        for k being the hash-keys of ht
        collect (funcall fn k v)))

(defield dictionary._keys ()
  (loop for k being the hash-keys of ( |DICTIONARY._HASH-TABLE| object)
        collect k))

(defield dictionary._try-key (key)
  "Unlike the ?field method, this always returns without error.  The 2nd value indicates that
KEY is contained in the dictionary."
  (gethash key ( |DICTIONARY._HASH-TABLE| object)))

(defconstant +dictionary-no-key+ (make-symbol "NO-KEY"))
(defield dictionary.?field ()
  (let ((result (gethash ?field ( |DICTIONARY._HASH-TABLE| object) +dictionary-no-key+)))
    (when (eq +dictionary-no-key+ result) 
      (b-error "Dictionary (~S) does not contain the key ~S." 
               object ?field))
    result))

(defield (setf dictionary.?field) (value)
  (setf (gethash ?field ( |DICTIONARY._HASH-TABLE| object)) value))


(defield dictionary._copy-from (d)
  "Copies the keys & values from another dictionary, d"
  (loop with ht = (|DICTIONARY._HASH-TABLE| d)
        with object-ht = ._hash-table
        for k being the hash-keys of ht
        for v being the hash-values of ht
        do (setf (gethash k object-ht) v)))

(defmethod describe-object ((d dictionary) stream)
  (princ d stream)
  (princ #\newline stream)
  (with-print-context d
    (loop with ht = ( |DICTIONARY._HASH-TABLE| d)
          for k being the hash-keys of ht
          for v being the hash-values of ht
          do (format stream  "~%~50S~S" k v))))
