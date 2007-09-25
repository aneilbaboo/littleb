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

;;; File: string-reader
;;; Description: String reading is required for some lisps

;;; $Id: string-reader.lisp,v 1.1 2007/09/25 17:54:13 amallavarapu Exp $
;;;
;;;

(in-package b)

(defun string-reader (stream char)
  (declare (ignorable char))
  (flet ((get-next-char ()
           (let ((c (read-char stream t nil t)))
             (cond
              ((char= c #\\)    (read-char stream t nil t))
              ((char= c #\")    nil)
              (t                c)))))
  (loop 
    with str = (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)
    for char = (get-next-char)
    while char
    do (vector-push-extend char str 8)
    finally (return (copy-seq str)))))

(set-macro-character #\" 'string-reader nil +b-readtable+)
