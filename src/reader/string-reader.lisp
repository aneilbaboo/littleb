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

;;; File: string-reader
;;; Description: String reading is required for some lisps

;;; $Id: string-reader.lisp,v 1.2 2008/09/02 14:58:11 amallavarapu Exp $
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
