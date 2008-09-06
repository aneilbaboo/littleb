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


;;; File: char-reader
;;; Description: char reading is required for some lisps

;;; $Id: char-reader.lisp,v 1.3 2008/09/06 00:23:09 amallavarapu Exp $
;;;
;;;

(in-package b)

(defun char-reader (stream char code)
  (declare (ignorable char code))

  (let* ((char1 (read-char stream t nil t))
         (str   (unless (terminator-char-p (peek-char nil stream nil #\space t))
                  (with-output-to-string (s)
                    (princ char1 s)
                    (loop with eof = '#:eof
                          for char = (read-char stream nil eof t)
                          when (terminator-char-p char)
                          do (unread-char char stream)
                             (loop-finish)
                          when (eq char eof)
                          do (loop-finish)
                          do (princ char s)
                          finally (return s))))))
    (cond
     (str
      (test (#'string= (string-downcase str))
        ("space" #\space)
        ("newline" #\newline)
        ("tab" #\tab)
        ("page" #\page)
        ("backspace" #\backspace)
        ("return" #\return)
        ("linefeed" #\linefeed)
        ("rubout" #\rubout)
        (t   (error "Wrong character name: ~@:(~A~)" str))))   
     (t char1))))

(set-dispatch-macro-character #\# #\\ 'char-reader +b-readtable+)

