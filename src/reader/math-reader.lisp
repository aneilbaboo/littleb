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

