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


;;; File: reify
;;; Description: enables temporary objects to be added to the DB

;;; $Id: reify.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $
;;; $NAme$
(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun reify (o)
  (kb-transaction
    (typecase o
      (concept  (reify-concept o))
      (cons     (reify-cons o))
      (sequence (map (type-of o) (lambda (el) (reify el)) o))
      (t        o))))

;;;; (defun reify-cons (o)  - inefficient and no tail optimization even when compiled
;;;;   (cons (reify (car o))
;;;;         (reify (cdr o))))

(defun reify-cons (o)
  "Used this instead of a tail-call optimizable recursion, since in interpreted mode, a
recursion would cause a lot of stack growth."
  (let* ((p        o)
         (butlast  (loop while (consp (cdr p))
                         collect (reify (first p))
                         do (setf p (cdr p))))
         (last     (cons (reify (car p)) (reify (cdr p)))))
    (nconc butlast last)))
        

(defun reify-concept (c)
  (let* ((hk       (concept-hashkey c))
         (r-hk     (reify hk))
         (mismatch (mismatch hk r-hk :test #'equalp))
         (existing (kb-find-object r-hk))
         (r-o      (cond    ;; the reified object
                    (existing existing)
                    (mismatch (cclass-create-from-hashkey r-hk))
                    (t        c))))

    (when (not existing)
      (kb-add-object r-hk r-o))

;;;;     (maphash (lambda (fld p) (declare (ignorable fld)) (reify-property p r-o))
;;;;              (concept-properties c))

;;;;     (map-plist (lambda (k i) (declare (ignorable k)) (reify-interface i r-o))
;;;;             (concept-interfaces c))
    
    r-o))

(defun reify-property (p r-o)
  "Where r-o is the reified object"
  (setf (property-object p) r-o)
  (let* ((p-hk      (property-hashkey p))
         (exist     (kb-find-object p-hk))
         (exist-rel (property-relevance exist))
         (p-rel     (property-relevance p))
         (r-p       (or exist p)))

    (setf (concept-property r-o) r-p)

    (cond
     ((or (not exist)  ;there's no existing property OR
          (and p-rel      ; relevance of new property is > existing
               (relevance> p-rel exist-rel :nil-policy :do-not-change)))
      (setf (property-value r-p) (reify (property-value p))
            (property-relevance r-p) p-rel))
     (t  (if exist exist
           (kb-add-object p-hk r-p))))))

;;;; (defun reify-interface (i r-o)
;;;;   (setf (interface-object i) r-o)
;;;;   (let ((i-hk (interface-hashkey i))
;;;;         (iclass (class-of i)))
;;;;     (unless (kb-find-object i-hk)
;;;;       (let ((newi (kb-add-object i-hk i)))
;;;;         (setf (concept-interface r-o iclass) newi)
;;;;         newi))))
)
