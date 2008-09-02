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

;;; File: error
;;; Description: condition and error functions for little b

;;; $Id: error.lisp,v 1.6 2008/09/02 14:58:10 amallavarapu Exp $
;;; $Name:  $
;;;
(in-package b)

(defvar *debugger-enabled* nil)
(defvar *last-error* nil)
(defvar *last-error-info* nil)

(defvar *b-error-context* ())

(defmacro with-b-error-context ((str &rest args) &body body)
  `(let ((*b-error-context* (list* (list ,str ,@args) *b-error-context*)))
     ,@body))

(defun littleb-debugger-hook (condition encapsulation)
  (declare (ignore encapsulation))
  (setf *last-error* condition)      
  #+:lispworks (setf *last-error-info* (dbg:bug-backtrace nil))
  (cond
   (*debugger-enabled*
    (invoke-debugger condition))
   (t 
    (format t "ERROR: ~A~%" condition *standard-output*)
    (invoke-restart 'abort))))

(defun b-format (stream string &rest args)
  (let ((*math-print-function* 'default-math-printer))
    (declare (ignorable *math-print-function*))
    (apply #'format stream string args)))

(define-condition b-error (error)
  ((format-string :initarg :format-string :accessor b-error-format-string :initform "")
   (format-arguments :initarg :format-arguments :accessor b-error-format-arguments :initform ())
   ;(cause :initarg :cause :accessor b-error-cause :initform ())
   )
  (:default-initargs :format-string "" :format-arguments ()) ; :cause ())
  (:report print-object))

(defmethod print-object ((o b-error) stream)
  (let ((*print-level* (if *print-level* (max *print-level* 15))))
    (cond 
     (*print-escape* (print-unreadable-object (o stream :type t :identity t)))
     (t  (let ((*print-context* t))
           (with-slots (format-string format-arguments ) o
             (apply #'format 
                    stream 
                    format-string
                    format-arguments)
             (format stream "~@[  CONTEXT:~{ ~A~^ ->~}~]"  
                     (mapcar (lambda (o) (handler-case (apply #'format nil o)
                                           (error (e) (format nil "Context unprintable: ~S" o ))))
                             (reverse *b-error-context*)))))))))

(defun b-error (str &rest args)
  (error 'b-error :format-string str :format-arguments args))

(defmacro b-assert (test-form &optional places datum &rest args)
  `(assert ,test-form ,places  ,@(if datum
                                   `((let ((*print-pretty* nil))
                                       (format nil ,datum ,@args))))))

(defun b-warn (str &rest args)
  (let ((*print-pretty* t)
        (*print-context* t))
    (apply #'warn str args)))


