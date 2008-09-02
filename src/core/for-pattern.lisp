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

;;; File: for-pattern
;;; Description: 

;;; $Id: for-pattern.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :Execute)

;;;
;;; FOR-PATTERN:  iterates over a sequence, evaluating a test form for each element of the sequence
;;;               when the test is successful, the result is stored.
;;;               
;;;
;;; (:for (var seq &key per per-test satisfy fail-value results)
;;;     form)
;;;
;;;; (defun rule-read-for-pattern (rule form var seq 
;;;;                                &key per (per-test 'equalp) 
;;;;                                (satisfy ''exactly-one-result-per-element)
;;;;                                (results (gensym "FOR-RESULTS")) ; a variable, which will hold the assoc-list
;;;;                                (fail-value nil)) ; the value returned by form which indicates test failure
;;;;   (let ((result          (gensym "RESULT"))
;;;;         (per-val         (gensym "PER"))
;;;;         (seq-val         (gensym "SEQ"))
;;;;         (fail-val        (gensym "FAIL-VALUE"))
;;;;         (changed         (gensym "CHANGED"))
;;;;         (per-ht          (make-hash-table :test per-test))
;;;;         (per-key-form    (or per seq))) ; for looking up the bindings on rule rhs
;;;;     (setf (gethash results (rule-for-results rule))
;;;;           (cons per-key-form per-ht))
;;;;     `((lisa:test 
;;;;        (let* ((,seq-val      ,seq)
;;;;               (,changed      nil)
;;;;               (,per-val      ,(or per seq-val))
;;;;               (,fail-val     ,fail-value))
;;;;          (map nil
;;;;               (lambda (,var)
;;;;                 (let ((,result ,form))
;;;;                   (when (not (eq ,result ,fail-val))
;;;;                     (setf ,changed t)
;;;;                     (record-for-result ,var ,result
;;;;                                        ,per-val ,per-ht))))
;;;;               ,seq-val)
;;;;          (when ,changed
;;;;            (funcall ,satisfy ,seq-val (gethash ,per-val ,per-ht))))))))


(defun record-for-result (el result key ht)
  "Records a satisfaction of a FOR pattern.  
    EL = the element for which the result was obtained, 
    KEY = the key under which the result should be stored, and 
    HT = the hashtable in which to store the results"
  (pushnew result (getf (gethash key ht) el)))

(defun at-least-one-result-per-element (list results)
  (eq (length results) ; results is a property list where each element of LIST is a key
      (* 2 (length list)))) ; if true, all elements have a binding
    
(defun exactly-one-result-per-element (list results)
  (when (at-least-one-result-per-element list results)
    (map-plist (lambda (k v)
                 (if (> (length v) 1) 
                     (b-error "More than one binding for ~S: ~S.  ~
                              Expecting exactly one result per element."
                              k v)))
               results)
    t))


;;;; (defun show-for-results (rule &optional (stream *standard-output*))
;;;;   (loop with for-results = (rule-for-results rule)
;;;;         for k being the hash-keys in for-results
;;;;         for v being the hash-values in for-results
;;;;         do (format stream "~S     ~S~%-----------------------------------------------------------------------------~%"
;;;;                    k (car v))
;;;;            (loop with perht = (cdr v)
;;;;                  for per being the hash-keys in perht
;;;;                  for results being the hash-values in perht
;;;;                  do (format stream "PER: ~S~%RESULTS: ~S~%" per results))))

)


;;;; (defun record-for-result (el result key ht)
;;;;   (push (cons el result) (gethash key ht)))

;;;; (defun exactly-one-result-per-element (list results)
;;;;   (let* ((result-keys     (mapcar #'first results))
;;;;          (extra-result    (find-extra result-keys list)))

;;;;     (when extra-result
;;;;         (b-error "More than one binding for ~S in :FOR pattern.  ~
;;;;                   Expecting exactly one result per element. ~%Results: ~S." 
;;;;                  extra-result results))
;;;;                  
;;;;     (eq (length list) (length results))))


;;;; (defun at-least-one-result-per-element (list results)
;;;;   (let* ((result-keys         (mapcar #'first results))
;;;;          (unsatisfied-element (find-extra list results)))
;;;;     (not unsatisfied-element)))
