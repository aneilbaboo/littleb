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

;;; File: reader-core
;;; Description: 

;;; $Id: reader-core.lisp,v 1.1 2007/09/25 17:54:12 amallavarapu Exp $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +standard-readtable+ (with-standard-io-syntax *readtable*))
(defconstant +b-readtable+ (copy-readtable *readtable*))
(defconstant +b-standard-tokens-readtable+ (copy-readtable *readtable*))
(defvar *working-readtable* nil
  "The working readtable is the 'true' little b readtable.")

(defconstant +limit-chars+ #(#\) #\] #\}))

(defconstant +dot-symbol-macro-chars+
  #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
    #\~ #\! #\@ #\$ #\% #\^ #\& #\* #\\ #\< #\> #\- #\_ #\+ #\= #\? #\/))

(defconstant +no-break-space-char+ (code-char 160)) ;; can't use "#\no-break-space" 
                                                    ;; because it is not ANSI-compliant
                                                    ;; e.g., CLISP uses "#\no_break-space"  instead,
                                                    ;; Allegro doesn't support it either.
    

(defconstant +whitespace-chars+
  `#(#\Space #\Return #\Linefeed #\Tab #\Newline #\Rubout #\Page ,+no-break-space-char+))

(defvar *terminator-chars*
  (concatenate 'list +whitespace-chars+ '(#\( #\) #\{ #\} #\[ #\] #\; #\' #\` #\,)))

(defconstant +sign-chars+
  #(#\- #\+))

(defconstant +exponent-chars+ #(#\e #\E #\s #\S #\f #\F #\d #\D))

(defun contains-char-p (c seq)
  (and c (if (find c seq :test #'safe-char=) t)))

(define-compiler-macro contains-char-p (&whole form c seq &environment env)
  (if (and (symbolp seq)
           (constantp seq env))
      `(case ,c
         (,(coerce (symbol-value seq) 'list) t))
    form)))

(defun safe-char= (c1 c2)
  (and (characterp c1)
       (characterp c2)
       (char= c1 c2)))

(defun limit-char-p (c)
  (contains-char-p c +limit-chars+))

(defun whitespace-char-p (c)
  (contains-char-p c +whitespace-chars+))

(defun terminating-macro-char-p (o)
  (and (characterp o)
       (multiple-value-bind (macrofn nonterm)
           (get-macro-character o)
         (and macrofn
              (not nonterm)))))

(defun terminator-char-p (c)
  (contains-char-p c *terminator-chars*))

(defun terminator-value-p (c)
  (or (null c) (terminator-char-p c)))

(defun comma-p (c)
  (safe-char= c #\,))

(defun dot-char-p (c)
  (safe-char= c #\.))

(defun list-start-p (c)
  (safe-char= #\( c))

(defun math-start-p (c)
  (safe-char= #\{ c))

(defun object-start-p (c)
  (safe-char= #\[ c))

(defun sign-char-p (c)
  (contains-char-p c +sign-chars+))

(defmacro with-standard-readtable (&body body)
  "More efficient than using (with-standard-io-syntax *readtable*), which must make a copy of the readtable each time."
  `(let ((*readtable* +standard-readtable+))
     ,@body))

(defun integer-string-p (str)
  "Strings which match the REGEXP: [+-]?[0123456789]+ 
I.e., a string which constitutes a valid integer or the part of a rational left of the dot char.
E.g., \"123\" or \"+123\" or \"-123\""
  (labels ((all-digit-chars-p (s)
             (every #'digit-char-p s)))
    (when str
      (or (all-digit-chars-p str)
          (and (sign-char-p (aref str 0))
               (> (length str) 1)
               (all-digit-chars-p (subseq str 1)))))))

(defun exponent-char-p (c)
  (contains-char-p c +exponent-chars+))

(defun fractional-string-p (str)
  "Strings which match the REGEXP: [012345679]*[[eEsSfFdD][+-]?[0123456789]+]
I.e., a string which represents the fractional part (right of the decimal) of a rational number.
E.g., \"123\" \"123e456\" \"123s-456\" as well as \"\""  
  (or (null str)
      (and (loop with state = :read-fraction
                 for i from 0 to (1- (length str))
                 for c = (char str i)
                 while (#!eq state :failed)                 
                 if (eq state :read-fraction)
                 do (cond
                     ((exponent-char-p c)    (setf state :read-exponent))
                     ((not (digit-char-p c)) (setf state :failed)))
                 if (eq state :read-exponent)
                 return (integer-string-p (subseq str (1+ i)))
                 finally (return (#!eq state :failed))))))
         
(defun maybe-token-terminator-p (c &optional left-chars)
  (declare (ignorable left-chars))
  (or (terminator-value-p c)
      (dot-char-p c)))

;;;; (defmacro brt (&body body)
;;;;   `(let ((*readtable* +b-readtable+)) ,@body))

;;;
;;; b-read-from-string: debugging macro
;;;
(defmacro b-read-string (str)
  `(let ((*readtable* +b-readtable+))
     (read-from-string ,str)))

(defun dot-or-terminator-p (c)
  (or (dot-char-p c) (terminator-char-p c)))


(defun consume-whitespace (stream)
  (loop for next = (peek-char nil stream nil nil t)
        while next
        if (whitespace-char-p next)
        do (consume-char stream)
        else do (return next)))

(defun move-to-next-valid-char (stream)
  (let ((next (consume-whitespace stream)))
    (cond
     ((safe-char= next #\;)
      (read-line stream nil nil t)
      (move-to-next-valid-char stream))
     (t next))))

(defun consume-char (stream)
  (read-char stream nil nil t))

(defun prepend-stream (str stream)
  "Prepends string to the stream (returns a concatenated stream)"
  (make-concatenated-stream (make-string-input-stream str)
                            stream))

