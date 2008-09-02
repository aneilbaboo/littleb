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

;;; File: lisa-extensions.lisp
;;; Description: alteration & extension of the LISA engine
;;;

;;; $Id: lisa-extensions.lisp,v 1.3 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $
(in-package lisa)



(portable:allowing-redefinitions 

(setf *allow-duplicate-facts* t)


(defun acquire-meta-data (actual-name)
  (labels ((build-meta-object (class all-superclasses) ;  NEW LINE (AM 9/19/03)
             (let* ((class-name (class-name class))
                    (meta-data
                     (make-fact-meta-object
                      :class-name class-name
                      :slot-list (class-get-slot-list class) ;;;; (reflect:class-slot-list class)
                      :superclasses all-superclasses))) ; new line (AM 9/19/03)
               (register-meta-object (inference-engine) class-name meta-data)
               meta-data))
           (examine-class (class-object)
             (let ((superclasses
                    (if *consider-taxonomy-when-reasoning*
                        (reflect:class-all-superclasses class-object) ; NEW LINE (AM 9/19/03)
                      nil)))
               (build-meta-object class-object superclasses)
               (dolist (super superclasses)
                 (examine-class super)))))
    (examine-class (find-class actual-name))))

(defmethod class-get-slot-list (class)
  (reflect:class-slot-list class))

(defmethod class-get-slot-list ((class b::concept-class))
  (loop for fi in (B::class-fieldinfos class)
        if (and (b::fieldinfo-matchable fi)
                (eq :id (b::fieldinfo-kind fi)))
        collect (b::fieldinfo-symbol fi)))

(defmethod slot-value-of-instance ((obj b::concept) field)
  (b::fld obj field))

(defmethod (setf slot-value-of-instance) (value (obj b::concept) field)
  (declare (ignore value obj field))
  (error "Not implemented"))

(lisa:clear)

(lisa:reset)

(setf (consider-taxonomy) t)
)