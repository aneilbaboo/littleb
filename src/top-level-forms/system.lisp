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

;;; File: system
;;; Description: misc system functions accessible to the user
;;;

;;; $Id: system.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $
;;;
(in-package b)


(defun retract (o)
  (if (conceptp o)
      (kb-retract o)
    (error "Attempt to retract ~A, which is not a concept object." o)))

(defmacro with-relevance (r &body body)
  `(let ((*relevance* ,r))
     ,@body))

(defun field-bound-p (obj fld)
  (ifit (fieldinfo obj fld)
      (let ((k (fieldinfo-kind it)))
        (ecase k
          (:property             (property-bound-p obj fld))
          ((:id :function)       t)))))

(defun property-bound-p (c p)
  "Returns T if property p is defined in concept c, or NIL.  An assertion is
triggered if P is not the name of a valid property in C."
  (concept-property-bound-p c p))
