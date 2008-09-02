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

;;; $Id: localization.lisp,v 1.4 2008/09/02 14:58:10 amallavarapu Exp $


;;; Description:  
;;; LOCALIZATION OBJECT: USED TO REPRESENT {entity @ location}
;;;

(in-package #I@library/biochem)

(defcon localization (:notrace)
  (entity location))

(defield localization.is-valid-for (loc-class)
  (unless (location-class-p loc-class)
    (b-error "Cannot determine whether localization ~S is valid -- invalid location-class (~S)"
             object loc-class))
  (let ((fi   (find .location loc-class._fieldinfos :key ?.symbol)))
    (and (typep fi 'fieldinfo)
         (subtypep (if (mutils:allow-type-p fi.type)
                       (mutils:allow-type-type fi.type) fi.type)
                   'location))))

(defmethod print-object ((o localization) stream)
  (let ((*print-context* t)) (print-math-expression o stream t)))

(defmethod print-math-expression ((o localization) &optional stream outer-op)
  (pprint-math-form `{,o.entity @ ,o.location} stream outer-op))


(defoperator _@ ((1- (operator-precedence '+)) :yfx)
    (rct loc-class)
  [localization rct loc-class])

(def-macro-operator @ ((1- (operator-precedence '+)) :yfx)
  (rct loc-class)
  (if (fld-form-p loc-class)
      `{{,rct _@ ,(fld-form-field loc-class) ,@(fld-form-args loc-class)}
        _@ ,(fld-form-object loc-class)}
    `{,rct _@ ,loc-class}))
