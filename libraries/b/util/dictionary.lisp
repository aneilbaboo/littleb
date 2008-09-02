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

;;; File: dictionary
;;; Description: the special dictionary object.  
;;;              Provides an object that has fields which are flexibly added.
;;;

;;; $Id: dictionary.lisp,v 1.9 2008/09/02 14:58:10 amallavarapu Exp $
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
               for ,valvar being the hash-values of (fld dict :_hash-table) ;changed to accomodate allegro
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
