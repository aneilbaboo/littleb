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


;;; File: for-pattern
;;; Description: 

;;; $Id: for-pattern.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $

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
