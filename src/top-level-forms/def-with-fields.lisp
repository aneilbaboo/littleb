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

;;; File: def-with-fields
;;; Description: adds fields support in defstruct-with-fields/defclass-with-fields
;;;              macros
;;;

;;; $Id: def-with-fields.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $
;;;
(in-package b)


(in-package :b)

(defmacro defstruct-with-fields (name-and-options &rest slot-descriptions)
  (let* ((name         (if (consp name-and-options) (first name-and-options) name-and-options))
         (deffld-forms (mapcan (lambda (sdescr)
                                 (let ((fname (if (consp sdescr) (first sdescr)
                                                sdescr)))
                                   `((defmethod fld ((object ,name) (f (eql ,(key fname))) 
                                                     &rest args)
                                       (declare (ignore f args))
                                       (slot-value object ',fname))
                                     (defmethod (setf fld)
                                       (value (object ,name) (f (eql ,(key fname))) 
                                              &rest args)
                                       (declare (ignore f args))
                                       (setf (slot-value object ',fname) value)))))
                               slot-descriptions)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1 (defstruct ,name-and-options ,@slot-descriptions)
       ,@deffld-forms))))
                       

(defmacro defclass-with-fields (name superclasses slots &rest class-options)
  (let ((deffld-forms (mapcan (lambda (slot)
                                (let ((fname (if (consp sdescr) (first sdescr)
                                               sdescr)))
                                  `((defmethod fld ((object ,name) (f (eql ,(key fname))) 
                                                    &rest args)
                                      (declare (ignore f args))
                                      (slot-value object ',fname))
                                    (defmethod (setf fld)
                                               (value (object ,name) (f (eql ,(key fname))) 
                                                      &rest args)
                                      (declare (ignore f args))
                                      (setf (slot-value object ',fname) value)))))
                              slots)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (prog1 (defclass ,name ,superclasses ,slots ,@class-options)
         ,@deffld-forms))))
                       