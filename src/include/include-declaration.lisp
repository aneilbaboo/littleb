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

         