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

;;; File: token-reader
;;; Description: 

;;; $Id: token-reader.lisp,v 1.2 2007/10/09 18:26:03 amallavarapu Exp $
(in-package b)

(defun token-reader (stream char)
  (let* ((tok  (read-token-string (prepend-stream (make-string 1 :initial-element char) stream)
                                  #'dot-or-terminator-p))
         (next (peek-char nil stream nil nil t))
         (head (with-standard-readtable (carefully-read-from-string-representing-stream tok stream))))
    (labels ((float-from-string (str stream) ;; reads a float from a string
               (read-with-fields (with-standard-readtable 
                                   (read-from-string str)) stream))
             (read-float-or-fld-form (head tok stream) ;; reads a float or a field expression
               (let* ((fractional-part? (read-token-string stream #'dot-or-terminator-p)))
                 (if (fractional-string-p (subseq fractional-part? 1))
                     (float-from-string (concatenate 'string tok fractional-part?) stream)
                   (read-with-fields head (prepend-stream fractional-part? stream)))))
             (maybe-float? () (and (integerp head) (dot-char-p next))))
      (cond
       ((maybe-float?)           (read-float-or-fld-form head tok stream))
       (t                        (read-with-fields head stream))))))
      
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