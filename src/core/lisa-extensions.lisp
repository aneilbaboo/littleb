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


;;; File: lisa-extensions.lisp
;;; Description: alteration & extension of the LISA engine
;;;

;;; $Id: lisa-extensions.lisp,v 1.4 2008/09/06 00:23:08 amallavarapu Exp $
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