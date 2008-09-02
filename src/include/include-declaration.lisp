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

(in-package b)

;;;;
;;;; include-declaration
;;;;
(defmacro include-declaration (&key expose-definitions export-definitions export-symbols expose-symbols use-packages expose-packages)
  (let* ((expose-definitions (ensure-list expose-definitions))
         (export-definitions (ensure-list export-definitions))
         (expose-symbols     (append (ensure-list expose-symbols) expose-definitions))
         (export-symbols     (append (ensure-list export-symbols) export-definitions))
         (definitions        (append expose-definitions export-definitions)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (def) `((define-global ,def ())
                                (declare-global-modular-definition ',def)))
                 definitions)
       (include-declaration-operation *package*
                                      :export-symbols ',export-symbols
                                      :expose-symbols ',expose-symbols
                                      :use-packages ',use-packages
                                      :expose-packages ',expose-packages))))

(defun include-declaration-operation (package 
                                      &key export-symbols expose-symbols use-packages expose-packages)
  (when use-packages    (use-package use-packages package))
  (when expose-packages (expose-package expose-packages package))
  (when export-symbols  (export export-symbols package))
  (when expose-symbols  (expose-symbol expose-symbols package))) 


(defun include-declaration-form-p (o)
  (and (consp o) (eq (first o) 'include-declaration)))

(defun declare-global-modular-definition (s)
  (setf (get s 'global-modular-location) t)
  (makunbound (make-global-place-symbol s)))

(defun update-global-modular-location (s)
  (let ((loc (get s 'global-modular-location)))
    (cond
     ((or (null loc)
          (equalp loc *load-truename*)) ())
     ((eq loc t)                        (setf (get s 'global-modular-location) *load-truename*))
     (t                                 (error "Attempt to redefine ~S in ~A.  (Previous definition in ~A)."
                                               s *load-truename* loc)))))

         