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

;;; File: field-reader
;;; Description: alters the lisp reader to deal with dot expressions:
;;;              e.g.,  x.y
;;;                     [x ...].y
;;;                     {...}.y, etc.

;;; $Id: field-reader.lisp,v 1.2 2007/11/12 15:06:09 amallavarapu Exp $
;;;
(in-package b)

(defun read-with-fields (form stream)
  "Provides support for field-access to form.  If form is followed (without whitespace) by a dot (.), 
it will be converted to a field expression.  form.X => (FLD form :X)"
  (let ((next (peek-char nil stream nil nil t)))
    (cond
     ((dot-char-p next)      (read-char stream nil nil t)
      (field-reader form stream))

     (t                      form))))  

(defun quote-form-p (o) (and (consp o) (eql (first o) 'quote)))

(defun field-reader (obj stream)
  (let ((escape-object *reading-comma-dot*))
    (labels ((invalid-field-expression (dot-follower)
               (b-reader-error stream "Invalid field expression.  ~S.~A not allowed." obj dot-follower))
             (read-field-token (next)
               (cond
                ((comma-p next)     (cond
                                     ((zerop *backquote-level*)
                                      (read-char stream t nil t)
                                      (if (safe-char= #\@ (peek-char nil stream nil nil t))
                                          (invalid-field-expression ",@")
                                        (read stream t nil t)))
                                     (t (read stream t nil t))))
              
                (t (let* ((tokstr   (read-token-string stream #'dot-or-terminator-p))
                          (tok      (handler-case (read-from-string tokstr nil nil :preserve-whitespace t)
                                      (error (e) (b-reader-error stream (format nil "~A" e))))))
                     (cond ; normalize the token
                      ((find #\: tokstr)          `(quote ,tok)) ; symbol with explicit package
                      ((and tok (symbolp tok))    (key tok))     ; symbol => keyword
                      (t                          tok))))))      ; otherwise, the TOK as read 
             (make-fld-form (obj ftok &optional args)
               (list* 'fld (if escape-object (cons '*comma* obj) obj)
                      ftok args)))
    
      (progv 
          (if *reading-comma-dot* '(*backquote-level* *reading-comma-dot*))
          (if *reading-comma-dot* (list (1+ *backquote-level*) nil))
        (move-to-next-valid-char stream)

        (let* ((*current-delimiter* #\.) ; suppresses comma-reader top-level error
               (next    (peek-char nil stream nil nil t))
               (new-obj (case next                    
                          (#\( 
                           (read-char stream t nil t)
                           (let ((fntok (read-field-token (peek-char nil stream t nil t)))
                                 (fnargs (read-dot-delimited-list #\) stream t)))
                             (make-fld-form  obj fntok fnargs)))

                          ((#\{ #\[)
                           (read-char stream t nil t)
                           (let ((signal (if (char= next #\{) ''*brace* ''*bracket*)))
                             (make-fld-form obj signal 
                                            (read-dot-delimited-list (closing-delimiter next) 
                                                                     stream t))))

                          ((#\`) (invalid-field-expression next))

                          (t   
                           (make-fld-form obj (read-field-token next))))))
        (cond
         ((dot-char-p (peek-char nil stream nil nil t))
          (consume-char stream)
          (field-reader new-obj stream))
         (t new-obj)))))))

;; replace all the token characters with the new token reader, which reads fields
(map 'simple-vector
     (lambda (c)
       (set-macro-character c 'token-reader t +b-readtable+))
     +dot-symbol-macro-chars+)


;; field-reader-wrapper  
(defun dispatch-field-reader (fn)
  (lambda (stream char n)
    (read-with-fields (funcall fn stream char n) stream)))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun enable-fields-on-dispatch-chars (&optional (disp-char (code-char 35)))
  (loop with std-table = (with-standard-io-syntax *readtable*)
        for code from 0 to 255
        for char = (code-char code)
        for fn = (get-dispatch-macro-character disp-char char std-table)
        when (and fn (not (member char '(#\+ #\- #\: #\# #\=))))
        do (set-dispatch-macro-character disp-char char (dispatch-field-reader fn) +b-readtable+))))
(enable-fields-on-dispatch-chars)
