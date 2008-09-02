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

(in-package :b)

(defvar *backquote-level* 0)

(defun bq-reader (stream char)
  (declare (ignorable char))
  (let ((*backquote-level* (1+ *backquote-level*))
        (*readtable* (copy-readtable))
        (*current-delimiter* nil)
        (next                (peek-char nil stream t nil t)))
    (set-dispatch-macro-character #\# #\( #'bq-vector-reader)
    (cond
     ((eql next #\.)
      (bq-expand (comma-reader stream #\.)))
     (t
      (bq-expand (read stream t nil t))))))

;;;;     (cond
;;;;      ((eql (peek-char nil stream t nil t) #\.)
;;;;       (read-char stream t nil t)
;;;;       (let ((fform (read stream t nil t)))
;;;;         (unless (fld-form-p fform)
;;;;           (b-reader-error stream "Expecting a field form following `."))
;;;;         (bq-expand (list* 'fld (cons '*comma* (fld-form-object fform))
;;;;                           (fld-form-field fform)
;;;;                           (fld-form-args fform)))))
;;;;      (t
;;;;       (bq-expand (read stream t nil t))))))

(defvar *reading-comma-dot* nil
  "True if the following expression is being read after ,.")

(defun comma-reader (stream char)
  (declare (ignorable char))
  (unless (> *backquote-level* 0)
    (b-reader-error stream "A comma appears outside the scope of a backquote (or there are too many commas)~@[ at ,~S~]" (ignore-errors (read stream nil nil t))))
  (let ((*backquote-level* (1- *backquote-level*))
        (signal            (case (peek-char nil stream t nil t)
                             (#\@ '*comma-at*)
                             (#\. '*comma-dot*) ; for fld-form substitution
                             (t   '*comma*))))
    (unless (eq signal '*comma*)
      (read-char stream t nil t))

    (when (and (eql signal '*comma-at*)
               (or *reading-cdr* 
                   (null *current-delimiter*)))                    
      (bq-comma-at-error stream))

    (cond
     (*reading-cdr*
      (list (cons '*comma-at* (read stream t nil t))))

     ((eq signal '*comma-dot*) 
      (let* ((*reading-comma-dot* t) ; a signal for field-reader
             (form (read stream t nil t)))
        (unless (fld-form-p form)
          (bq-comma-dot-error stream))
        form))

     (t 
      (cons signal
            (read stream t nil t))))))

(defun bq-comma-dot-error (stream)
  (b-reader-error stream "Expecting a field form after ,."))

(defun comma-form-p (x) (and (consp x) (eq '*comma* (car x))))
(defun comma-at-form-p (x) (and (consp x) (eq '*comma-at* (car x))))
(defun comma-dot-form-p (x) (and (consp x) (eq '*comma-dot* (car x))))
(defun dot-comma-form-p (x) (and (consp x) (eq '*dot-comma* (car x))))
;;;; (defun backquote-form-p (x) (and (consp x) 
;;;;                                  (member (car x)
;;;;                                          '(bq-vector bq-list bq-list* bq-append))))
(defun any-comma-form-p (x) (and (consp x) (member (car x) '(*comma* *comma-at* *dot-comma*))))
(defun dotted-list-p (x) (and (consp x) (cdr (last x))))  
(defun dotted-cons-p (x) (and (consp x) (not (listp (cdr x)))))
(defun bq-vector-form-p (x) (and (consp x) (eq 'bq-vector (car x))))


(deftype comma-form () '(satisfies comma-form-p))
(deftype comma-at-form () '(satisfies comma-at-form-p))
(deftype dot-comma-form () '(satisfies dot-comma-form-p))
(deftype comma-dot-form () '(satisfies comma-dot-form-p))
;(deftype backquote-form () '(satisfies backquote-form-p))
(deftype any-comma-form () '(satisfies any-comma-form-p))
(deftype constant () '(satisfies constantp))                             
(deftype bq-vector-form () '(satisfies bq-vector-form-p))

(defun bq-expand (x)
  (etypecase x
    (comma-form     (remove-comma x))
    (comma-at-form  (bq-comma-at-error *error-output*))        
    (vector         (if (needs-bq-expansion x) (bq-vector-expand x)
                      (quote-form x)))                        
    (cons           (if (needs-bq-expansion x) (bq-expand-list x)
                      (quote-form x)))
    (constant       x)
    (symbol         (quote-form x))))

(defun remove-comma (form) (assert (any-comma-form-p form)) (cdr form))

(defun quote-form (form) (list 'quote form))
           
(defun needs-bq-expansion (x)
  (typecase x
    ((or comma-form 
         comma-at-form) t)
    (cons   (or (needs-bq-expansion (first x))
                (needs-bq-expansion (rest x))))
    (vector (some #'needs-bq-expansion x))))

(defun bq-expand-list (x)
  (let ((first (first x)))
    (cond
     ((comma-at-form-p first) (bq-expand-splice first (rest x)))
     (t
      (let ((operator (if (or (dotted-list-p x)
                              (some #'comma-at-form-p x)) 
                          'bq-list* 'bq-list)))
        (list* operator (bq-expand-rest x)))))))

(defun bq-comma-at-error (stream)
  (unless *read-suppress*
    (b-reader-error stream ",@ not allowed after . or `")))

(defun bq-expand-splice (first rest)
  "Returns the appropriate form to splice first onto any elements in REST"
  (assert (comma-at-form-p first))
  (let ((val (remove-comma first)))
   (if rest (list* 'bq-append val
                   (bq-append-rest rest))
     val)))

(defun bq-vector-expand (x)
  "Takes a vector which may have comma forms in it, and expands it"
  `(bq-vector ,(second (type-of x)) ,(bq-expand (coerce x 'list))))

(defun bq-append-rest (list)
  "Computes the arguments to APPEND"
  (cond 
   ;; when we're appending (... *COMMA* . form)
   ((comma-form-p list) (list (remove-comma list)))
   (t
    (let ((first (first list)))
      (cond 
       ((comma-at-form-p list)  (error ",@ not allowed after . or `"))
       ((comma-at-form-p first) (let ((rest (rest list)))
                                  (cons (remove-comma first)
                                        (if rest
                                            (bq-append-rest rest)))))
       (t                       (list (bq-expand-list list))))))))

(setf (symbol-function 'bq-append) #'append
      (symbol-function 'bq-list) #'list
      (symbol-function 'bq-list*) #'list*)

(defun bq-expand-rest (x)
  "Returns the expanded, evaluatable form of the rest of the items of a list"
  (let ((first (first x))
        (rest  (rest x)))
    (cond 
     ;; . ,@ - error
     ((comma-at-form-p x)      (bq-comma-at-error *standard-input*))

     ;; ,@ - start splicing
     ((comma-at-form-p first)  (list (bq-expand-splice first rest)))
     
     ;; end of list, just expand the last element
     ((null rest)              (list (bq-expand first)))

     ;; last elt of dotted list... (a b ,c . d) => (list* 'a 'b c 'd)
     ((dotted-cons-p x)        (list (bq-expand first) (bq-expand rest)))
     
     ;; dotted list with comma. E.g., (a b c . ,d) => (list* 'a 'b 'c d)
     ((comma-form-p rest)      (list (remove-comma rest)))

     ;; just a regular list - expand it
     (t                        (cons (bq-expand first)
                                     (bq-expand-rest rest))))))
  
(set-macro-character #\` 'bq-reader nil +b-readtable+)
(set-macro-character #\, 'comma-reader nil +b-readtable+)

(defun bq-vector-reader (stream char n)
  (declare (ignorable char))
  (unread-char #\( stream)
  (let ((elts (read stream t nil t)))
    (bq-vector n elts)))

(defun bq-vector (n elts) 
  (let ((len  (length elts)))
    (apply #'vector 
           (cond
            ((null n)  elts)
            ((> n len) (nconc elts
                              (make-list (- n len)
                                         :initial-element
                                         (first (last elts)))))
            ((< n len) (error "Vector longer than specified length: #~A(~{~S~^ ~})"
                              n elts))
            (t         elts)))))
           
