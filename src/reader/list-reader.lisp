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

;;; File: list-reader
;;; Description: replaces the lisp list-reader so that lists can participate
;;;              in dot expressions.  E.g., (build-object).access-field

;;; $Id: list-reader.lisp,v 1.3 2008/09/02 14:58:11 amallavarapu Exp $(in-package b)

(in-package b)

(defconstant +lisp-list-reader+ (with-standard-readtable 
                                  (set-syntax-from-char +no-break-space-char+ #\space)
                                  (get-macro-character #\()))


(defconstant +consing-dot+ (make-symbol "."))
(defconstant +limit-char+ (make-symbol "LIMIT"))
(defvar *delimiters* '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))
(defvar *current-delimiter* ())

(defun closing-delimiter (x)
  (cdr (assoc x *delimiters* :test #'eql)))

(defun consing-dot-p (c)
  (eq c +consing-dot+))

;;;
;;; a limit signal indicates that a special character
;;;   representing a limit has been read
;;;
(defun limit-signal-p (o)
  (and (consp o)
       (eq (car o) +limit-char+)))
(defun limit-signal-char (o)
  (cdr o))
(defun limit-signal (o)
  (cons +limit-char+ o))


(defun list-reader (stream char)
  (declare (ignorable char))
  (let ((*current-delimiter* char))
    (read-with-fields (read-dot-delimited-list (closing-delimiter char) stream t) stream)))

(defun dot-token-reader (stream char)
  (declare (ignorable char))
  (let ((next (peek-char nil stream nil nil t)))
    (cond
     ((terminator-value-p next)
      (unless *current-delimiter* (dot-context-error stream))
      +consing-dot+)

     (t                         (token-reader stream #\.)))))

(defun limit-char-reader (stream char)
  (let ((signal (limit-signal char)))
    (cond
     ((null *current-delimiter*) 
      (unexpected-limit-signal signal stream))

     ((not (char= *current-delimiter* char))
      (unexpected-limit-signal signal stream))

     (t signal))))

(defun b-reader-error (stream &rest args)
  (cond
   #+(and :lispworks (not :delete-lw-editor))
   ((interactive-stream-p stream)
    (fresh-line stream)
    (princ "Error while reading: " stream)
    (apply #'format stream args)
    (fresh-line stream)
    (let* ((*read-suppress* t))
      (read-line stream nil nil t))
    (apply #'b-reader-error stream args))

   #+(and :lispworks (not :delete-lw-editor))  
   ((typep stream 'concatenated-stream)
    (let ((istream (find-if #'interactive-stream-p 
                            (concatenated-stream-streams stream))))
      (apply #'b-reader-error istream args)))

   (t (apply #'error args))))

(defun dot-context-error (stream)
  (b-reader-error stream "Dot context error."))

(defun unexpected-limit-signal (signal stream)
  (b-reader-error stream "Unexpected closing ~A encountered."
                (limit-signal-char signal))
  signal)

(defun read-dot-delimited-list (limit stream &optional recp)
  (cond
   (*read-suppress* (read-delimited-list limit stream recp))
   (t               (read-dot-delimited-list-internal limit stream recp))))
  
(defvar *reading-cdr* ())

(defun read-dot-delimited-list-internal (limit stream &optional recp)
  "Returns 2 values: LIST, IS-DOTTED"
  (labels ((read-dotted-list-cdr ()
             (let* ((*reading-cdr* t)
                    (cdr (read stream nil nil recp))
                    (signal (read stream nil nil recp)))
               (cond
                ;; correct limit, but encountered too soon
                ((limit-p cdr) (dot-context-error stream))
                ;; incorrect limit
                ((limit-signal-p cdr) (unexpected-limit-signal cdr stream))
                ;; extra consing dot
                ((consing-dot-p cdr) (dot-context-error stream))
                ;; doesn't end with the limit
                ((not (limit-signal-p signal)) (dot-context-error stream))
                ;; ends with the incorrect limit
                ((not (limit-p signal)) (unexpected-limit-signal signal stream))
                ;; SUCCESS: return the cdr
                (t cdr))))
           (limit-p (o) (and (limit-signal-p o) 
                             (char= (limit-signal-char o) limit)))
           (check-limit (o) (when (limit-signal-p o)
                              (if (char= (limit-signal-char o) limit) t
                                (unexpected-limit-signal o stream)))))
    (let* ((*reading-cdr*              nil)
           (*current-delimiter*        limit)
           (first                      (read stream t nil recp)))           
      (cond ;; special handling for start of list:
       ;; () => NIL
       ((limit-p first)        (values nil nil))
    
       ;; first elt of list may not be the consing DOT
       ((consing-dot-p first)  (dot-context-error stream))

       ;; the main list reading loop:
       (t
        (loop with list = (cons first nil)
              for last = list 
              then (setf (cdr last) (cons car nil))              
              for car = (read stream t nil recp)

              until (check-limit car)

              ;; read dotted list
              when (consing-dot-p car)
              do   (setf (cdr last) (read-dotted-list-cdr))
              (return (values list t))
             
              finally (return (values list nil))))))))

(defun nreverse-dotted-list (list cdr)
  (let ((rlist (nreverse list)))
    (setf (cdr (last rlist)) cdr)
    rlist))

(defun reverse-dotted-list (list cdr)
  (let ((rlist (reverse list)))
    (setf (cdr (last rlist)) cdr)
    rlist))

(set-syntax-from-char +no-break-space-char+ #\space +b-readtable+ +b-readtable+)
(set-macro-character #\( #'list-reader nil +b-readtable+)
(set-macro-character #\) #'limit-char-reader nil +b-readtable+)
(set-macro-character #\. #'dot-token-reader t +b-readtable+)
