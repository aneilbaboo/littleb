;;;; This file is part of little b.

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

;;; File: object-operator
;;; Description: [] := The object expression operator macro 
;;;

;;; $Id: object-reader.lisp,v 1.2 2007/12/17 00:59:02 amallavarapu Exp $
(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(set-syntax-from-char #\[ #\( +b-readtable+)
(set-syntax-from-char #\] #\) +b-readtable+)
 
(defun object-macro-reader (stream char)
  "[ macro-character reader - reads regular [...] object expressions."
  (declare (ignorable char))
  (let ((expr (read-dot-delimited-list #\] stream t)))
    (read-with-fields `(OBJECT ,@expr) stream)))


(defun temp-object-macro-reader (stream char n)
  "#[ dispatch macro-character reader - reads temporary object expressions #[...]"
  (declare (ignorable char n))
  (let ((expr (read-dot-delimited-list #\] stream t)))
    (read-with-fields `(TEMPORARY-OBJECT ,@expr) stream)))


(set-dispatch-macro-character #\# #\[ #'temp-object-macro-reader +b-readtable+)
(set-macro-character #\[ #'object-macro-reader nil +b-readtable+)
(set-macro-character #\] #'limit-char-reader nil +b-readtable+)

#+clisp
(progn
  (set-dispatch-macro-character #\# #\[ #'temp-object-macro-reader +b-standard-tokens-readtable+)
  (set-macro-character #\[ #'object-macro-reader nil +b-standard-tokens-readtable+)
  (set-macro-character #\] #'limit-char-reader nil +b-standard-tokens-readtable+))

)



