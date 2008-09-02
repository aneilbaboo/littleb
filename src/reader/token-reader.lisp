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

;;; File: token-reader
;;; Description: 

;;; $Id: token-reader.lisp,v 1.5 2008/09/02 14:58:11 amallavarapu Exp $
(in-package b)

(defun token-reader (stream char &optional (object-package *package*))
  (let* ((tok  (read-token-string (prepend-stream (make-string 1 :initial-element char) stream)
                                  #'dot-or-terminator-p))
         (next (peek-char nil stream nil nil t))
         (head (let ((*readtable* +standard-readtable+)
                     (*package* object-package))
                 (carefully-read-from-string-representing-stream tok stream))))
    (labels ((float-from-string (str stream) ;; reads a float from a string
               (read-with-fields (with-standard-readtable                                    
                                     (read-from-string str))
                                 stream))
             (read-float-or-fld-form (head tok stream) ;; reads a float or a field expression
               (let* ((fractional-part? (read-token-string stream #'dot-or-terminator-p)))
                 (if (fractional-string-p (subseq fractional-part? 1))
                     (float-from-string (concatenate 'string tok fractional-part?) stream)
                   (read-with-fields head (prepend-stream fractional-part? stream)))))
             (maybe-float? () (and (integerp head) (dot-char-p next))))
      (cond
       ((maybe-float?)           (read-float-or-fld-form head tok stream))
       (t                        (read-with-fields head stream))))))
      
#+:clisp
(progn
(defun uninterned-reader (stream char n)
  (declare (ignore char n))
  (let* ((tok    (read-token-string (prepend-stream "#:" stream)
                                 #'dot-or-terminator-p))
         (*readtable* +standard-readtable+))
      (values (read-from-string tok))))
(set-dispatch-macro-character #\# #\: 'uninterned-reader +b-readtable+))

(defun keyword-reader (stream char)
  (declare (ignorable char))
  (read-with-fields (let ((*readtable* +standard-readtable+))
                      (read-from-string (read-token-string (prepend-stream ":" stream)
                                                           #'dot-or-terminator-p)))
                    stream))

(defun carefully-read-from-string-representing-stream (str stream)
  (handler-case (read-from-string str)
    (error (e) (b-reader-error stream "~A" e))))

(defun read-token-string (stream terminator-char-pred)
  (let+ (((token-str colon-pos) (collect-token-chars stream terminator-char-pred)))
    (resolve-token-package token-str colon-pos)))
  
(defun collect-token-chars (stream terminator-char-pred)
  (let ((colon-pos nil))
    (values (with-output-to-string (str)
              (loop with char0 = (read-char stream nil nil t)
                    for last-char = nil then char
                    for char = char0 then (read-char stream nil nil t)
                    for terminator = (if (funcall terminator-char-pred char)
                                         (if (and last-char (char= last-char #\:))
                                             (b-reader-error
                                              stream "~S is invalid after a colon." char)
                                           t))
                    for continue = char then (and char 
                                                  (or (not terminator)
                                                      (char= last-char #\\)))
                    for pos = 0 then (1+ pos)
                    while continue
                    do (princ char str)
                    if (and (char= char #\:) (null colon-pos))
                    do (setf colon-pos pos)
                    if (char= char #\|)
                    do (loop do (setf last-char char
                                      char (read-char stream nil nil t)
                                      continue (and char (not (char= char #\|))))
                                (princ char str)
                             while continue)
                    finally (if char (unread-char char stream))))
            colon-pos)))

 

(defvar *local-nicknames* ())

(defmacro use-package-nicknames (&rest args)
  "Locally nicknames other packages"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (mapp (lambda (package nickname)
             (pushnew (cons (mkstr nickname) (mkstr package) )
                      *local-nicknames*
                      :key #'car :test #'string=))
           ',args)
     *local-nicknames*))

(defun clear-nicknames () (setf *local-nicknames* nil))

(defun find-true-package-name (pkg-name)
  (or (cdr (assoc pkg-name *local-nicknames* :test #'string-equal))
      pkg-name))

(defun resolve-token-package (token-str colon-pos)
  (cond 
   (colon-pos (let* ((rel-pkg-name  (subseq token-str 0 colon-pos))
                     (true-pkg-name (find-true-package-name rel-pkg-name)))
                (concatenate 'string true-pkg-name (subseq token-str colon-pos))))
   (t         token-str)))


#+clisp 
(progn ; clisp's #: reader doesn't like that i've reassigned the token-reader fn.
(defun hash-colon-reader (stream char x)
  (declare (ignorable char x))
  (read-with-fields 
   (with-standard-io-syntax
     (read-from-string (format nil "#:~A" (collect-token-chars stream #'dot-or-terminator-p))))
   stream))

(set-dispatch-macro-character #\# #\: 'hash-colon-reader +b-readtable+)
(set-macro-character #\| 'token-reader t +b-readtable+))

(set-macro-character #\: 'keyword-reader t +b-readtable+)
