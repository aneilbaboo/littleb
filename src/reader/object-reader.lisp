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

;;; File: object-operator
;;; Description: [] := The object expression operator macro 
;;;

;;; $Id: object-reader.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $
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



