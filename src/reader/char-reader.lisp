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

;;; File: char-reader
;;; Description: char reading is required for some lisps

;;; $Id: char-reader.lisp,v 1.2 2008/09/02 14:58:11 amallavarapu Exp $
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

