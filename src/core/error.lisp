;;; This file is part of the little b (b) language.

;;;; The MIT License

;;;; Copyright (c) 2007 Aneil Mallavarapu

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

;;; File: error
;;; Description: condition and error functions for little b

;;; $Id: error.lisp,v 1.3 2007/11/12 15:00:29 amallavarapu Exp $
;;; $Name:  $
;;;
(in-package b)

(defun b-format (stream string &rest args)
  (let ((*math-print-function* 'default-math-printer))
    (apply #'format stream string args)))

(define-condition b-error (error)
  ((format-string :initarg :format-string :accessor b-error-format-string :initform "")
   (format-arguments :initarg :format-arguments :accessor b-error-format-arguments :initform ())
   (cause :initarg :cause :accessor b-error-cause :initform ()))
  (:default-initargs :format-string "" :format-arguments () :cause ())
  (:report print-object))

(defmethod print-object ((o b-error) stream)
  (let ((*print-level* (if *print-level* (max *print-level* 15))))
    (cond 
     (*print-escape* (print-unreadable-object (o stream :type t :identity t)))
     (t  (let ((*print-context* t))
           (with-slots (format-string format-arguments cause) o
             (apply #'format 
                    stream 
                    format-string
                    format-arguments)
             (when cause
               (princ "  CAUSE: " stream)
               (if (stringp cause) (princ cause stream)
                 (prin1 cause stream)))))))))
(defun b-error (str &rest args)
  (error 'b-error :format-string str :format-arguments args))

(defmacro b-assert (test-form &optional places datum &rest args)
  `(assert ,test-form ,places   (let ((*print-pretty* nil))
                                  (format nil ,datum ,@args))))

(defun b-warn (str &rest args)
  (let ((*print-pretty* t)
        (*print-context* t))
    (apply #'warn str args)))

