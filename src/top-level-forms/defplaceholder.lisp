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

;;; File: defplaceholder
;;; Description: enables symbols to be defined modularly - 
;;;              var is declared in one location (defplaceholder)
;;;              alternate values are provided in different locations (defplaceholder-value)

;;; $Id: defplaceholder.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $

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



