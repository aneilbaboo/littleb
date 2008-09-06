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


;;; File: defplaceholder
;;; Description: enables symbols to be defined modularly - 
;;;              var is declared in one location (defplaceholder)
;;;              alternate values are provided in different locations (defplaceholder-value)

;;; $Id: defplaceholder.lisp,v 1.4 2008/09/06 00:23:09 amallavarapu Exp $

(in-package b)

(portable:define-dspec-class defplaceholder () "")
(portable:define-dspec-form-parser defplaceholder (name)
  (declare (ignorable defplaceholder))
  `(defplaceholder ,name))

(defmacro defplaceholder (name &optional documentation)
  (let ((placeholder-dspec `(defplaceholder ,name)))
    `(portable:dspec ,placeholder-dspec
       (portable:destroy-dspec `(defplaceholder-value ,',name)) ; clear the location of the value
       (defvar ,name ,@(when documentation `(nil ,documentation)))
       (export ',name)
       (makunbound ',name)
       (setf (get ',name 'placeholder) :undefined)
       (portable:record-definition ',placeholder-dspec (portable:location) t)
       ',name)))

(port:setup-indent 'defplaceholder 1 2 4)

(defun check-placeholders-defined (symbols error-format-string &rest format-args)
  (unless (every (lambda (symbol)
                   (let ((definition (get symbol 'placeholder)))
                     (and definition (not (eql definition :undefined)))))
                 (ensure-list symbols))
    (apply #'error error-format-string format-args)))

(port:setup-indent 'check-placeholders-defined 1 2 4)

(portable:define-dspec-class defplaceholder-value () "")
(portable:define-dspec-form-parser defplaceholder-value (name)
  `(defplaceholder-value ,name))

(defmacro defplaceholder-value (name value)
  (let ((dspec `(defplaceholder-value ,name)))
    `(portable:dspec ,dspec
       (record-placeholder-value-location ',name (current-source-include-path))
       (portable:record-definition `(defplaceholder-value ,',name) (portable:location) t)
       (export ',name)
       (setf ,name ,value)
       ',name)))

(defun record-placeholder-value-location (name location)
  (let ((defined-location (get name 'placeholder)))
    (unless (or (eql defined-location :undefined)
                (equalp defined-location location))
      (b-error "Attempt to redefine ~S in ~S (previously defined in ~S)." location defined-location))
    (setf (get name 'placeholder) location)))


;;;; (defun dspec-first-location (dspec)
;;;;   (second (first (portable:find-dspec-locations dspec))))

;;;; (defun dspec-include-path (dspec)
;;;;   (ignore-errors (include-path-from-pathname (dspec-first-location dspec))))

;;;; (defun dspec-include-path-loaded-p (dspec)
;;;;   (ignore-errors (include-path-current-signature (dspec-include-path dspec))))



