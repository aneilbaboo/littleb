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

;;; File: reify
;;; Description: enables temporary objects to be added to the DB

;;; $Id: reify.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $
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
