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


;;; File: def-with-fields
;;; Description: adds fields support in defstruct-with-fields/defclass-with-fields
;;;              macros
;;;

;;; $Id: def-with-fields.lisp,v 1.4 2008/09/06 00:23:09 amallavarapu Exp $
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
                       