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
(declaim (special *include-verbose*))
(defun default-curly-brace-reader (stream char)
  "Ensures that b/math is loaded, then calls curly-brace-reader"
  (declare (special *include-suppress*))
  (let ((*include-verbose* nil)
        (*load-verbose* nil))
    (unless (or *read-suppress* *include-suppress*)
      (include-dynamic 'b/math nil :verbose nil))
    (curly-brace-reader stream char)))

(defun curly-brace-reader (stream char)
  (declare (ignorable char))
  (let ((expr (read-dot-delimited-list #\} stream t)))
    (read-with-fields `(math ,@expr) stream)))

(defun math-form-expand (o &optional environment)
  (let+ ((exp (macroexpand o environment)))
    (if (eq (length exp) 2) (second exp)
      exp)))

(defun math-form-p (o)
  (and (consp o)
       (eq (first o) 'math)))

(defun math-setf-form-p (o)
  (when (math-form-p o)
    (let ((code (math-form-code o)))
      (and (= 3 (length code))
           (eq (second code) :=)))))

(defun math-setf-form-place (o)
  (first (math-form-code o)))

(defun math-setf-form-value (o)
  (third (math-form-code o)))

(defun math-form-code (o)
  "List representing the infix expression to be evaluated."
  (rest o))

(deftype math-form ()
  `(satisfies math-form-p))

(deftype math-setf-form ()
  `(satisifies math-setf-form-p))

(defun set-default-curly-brace-reader (rt)
  (set-syntax-from-char #\{ #\( rt)
  (set-syntax-from-char #\} #\) rt)
  (set-macro-character #\{ #'default-curly-brace-reader nil rt)
  (set-macro-character #\} #'limit-char-reader nil rt))

(set-default-curly-brace-reader +b-readtable+)
#+clisp
(set-default-curly-brace-reader +b-standard-tokens-readtable+)

