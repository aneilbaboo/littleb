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



;;; File: printer.lisp
;;; Description: print methods for little b forms - 
;;;              fld-forms x.y, math-forms {}, object-forms []
;;;              as well as replacements for the backquote, and quote
;;;              printers
;;;
(in-package :b)


;; +top-level-pprint-dispatch-table+ is needed for correct CLISP printing
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +top-level-pprint-dispatch-table+ 
    (with-standard-io-syntax (copy-pprint-dispatch)))

  (defmacro with-platform-printing-environment (&body body)
    #+:clisp  (let ((block-name '#:PPRINT-BLOCK-NAME)
                    (list-name  '#:LIST))
                `(let ((*readtable* +standard-readtable+))
                   (macrolet ((pprint-logical-block ((stream 
                                                      list 
                                                      &key
                                                      (prefix nil prefixp) 
                                                      (suffix nil suffixp)
                                                      (per-line-prefix nil plprefixp))
                                                     &body pbody)
                                (let ((,block-name (gensym "PPRINT-BLOCK-NAME")))
                                  `(block ,',block-name
                                     (let ((,',list-name ,list))
                                       (macrolet ((pprint-pop () `(pop ,',',list-name)))
                                         (flet ((pprint-exit-if-list-exhausted ()
                                                  (if (null ,',list-name) (return-from ,',block-name))))
                                           ,(if prefixp `(princ ,prefix ,stream))
                                           ,(if plprefixp (error "Per-line-prefix not allowed here"))
                                           ,@pbody
                                           ,(if suffixp `(princ ,suffix ,stream)))))))))
                     ,@body)))
    #-:clisp `(progn ,@body))

  (defmacro def-pprint-dispatcher (name (type &optional (priority 0)) lambda-list &body body)
    `(progn (defun ,name ,lambda-list
              (with-platform-printing-environment ,@body))
       (set-pprint-dispatch ',type ',name ,priority
                            +top-level-pprint-dispatch-table+))))
  
		
;;;
;;; PRINT METHODS
;;;
(defun pprint-newline-selectively (kind &optional (stream *standard-output*))
  "Prints a newline only when *print-pretty* is true."
  #+:clisp (declare (ignore kind stream))
  #-:clisp (when *print-pretty* (pprint-newline kind stream)))

(defconstant +special-cons-printers+ (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro def-cons-printer (head lambda-list &body body)
  (let ((printer-name (sym head '-cons-printer)))
    `(progn (defun ,printer-name ,lambda-list
              ,@body)
       ,(etypecase head
         (cons
          `(set-pprint-dispatch ',head ',printer-name 0 +top-level-pprint-dispatch-table+))
         (symbol
          `(setf (gethash ',head +special-cons-printers+)
                 ',printer-name))))))
)

(set-pprint-dispatch 'cons 'b-pprinter 99 +top-level-pprint-dispatch-table+)


(defun b-pprinter (stream o) (print-b-object o stream))

(defgeneric print-b-object (o &optional stream)
  (:method (o &optional stream) (print-object o stream)))

(defmethod print-b-object ((o cons) &optional stream)
  (let ((special (gethash (first o) +special-cons-printers+)))
    (if special (funcall special stream o)
      (cons-printer stream o))))

;;; QUOTE PRINTER
(deftype quote-form () '(cons (eql quote) *))
     
(def-cons-printer quote (stream o)
  (princ #\' stream)
  (prin1 (second o) stream))

;;; 
;;; OBJECT FORM PRINTER
;;;
(def-cons-printer object (stream oform)
  (cons-printer stream (rest oform) "[" "]"))

;;; 
;;; MATH FORM PRINTER
;;;
(def-cons-printer math (stream oform)
  (cons-printer stream (rest oform) "{" "}"))



;;;
;;; COMMA-PRINTERS
;;;
(defvar *bq-printer-level* 0)
(declaim (special *printing-fld-form-object*))
(def-cons-printer *comma* (stream o)
  (print-comma-form "," o stream))
(def-cons-printer *comma-at* (stream o)
  (print-comma-form ",@" o stream))
(def-cons-printer *dot-comma* (stream o)
  (print-comma-form ". ," o stream))
(def-cons-printer *comma-dot* (stream o)
  (print-comma-form ",." o stream))

(defun print-comma-form (str o stream)
  (cond
   ((zerop *bq-printer-level*)
    (cons-printer stream o))
   (t
    (let ((*bq-printer-level* (1- *bq-printer-level*)))
      (progv 
          (if *printing-fld-form-object* '(*printing-fld-form-object*))
          '(nil)
        (format stream "~A~S" str
                (cdr o)))))))

;;; BQ-VECTOR-PRINTER
(def-cons-printer bq-vector (stream o)
  (cons-printer stream (third o) (format nil "#~@[~A~](" (cddr o)) ")"))


(def-cons-printer bq-list (stream list)
  (bq-printer stream list))

(def-cons-printer bq-list* (stream list)
  (bq-printer stream list))

(def-cons-printer bq-append (stream list)
  (bq-printer stream list))

(defun bq-printer (stream list)
  (let ((*print-context* list)
        (*bq-printer-level* (1+ *bq-printer-level*)))
    (cond
     (*print-pretty*        
;      (with-bq-printers
        (princ #\` stream)
        (let ((item (printable-bq-item list)))
          (etypecase item
            (fld-form  (fld-form-printer stream item))
            (simple-vector (prin1 item stream))
            (cons      (cons-printer stream item)))))
     (t (cons-printer stream list)))))


(defun printable-bq-item (o &optional (bq-op '*comma*))
  (labels ((build-appended (o op last-op)
             (let* ((butlast (butlast o))
                    (last    (printable-bq-item (first (last o)) last-op))
                    (fixed-last (if (any-comma-form-p last) (list last) last)))
               (nconc (mapcar (lambda (x) (printable-bq-item x op)) butlast)
                      fixed-last)))
           (make-printable-bq-vector (n elts)
             (bq-vector n  (ifit (atom elts) (list (cons '*comma-at* elts))
                             (printable-bq-item elts)))))
    (cond
     ((quoted-p o)  (second o))
     ((consp o)
      (case (first o)
        (bq-vector (make-printable-bq-vector (second o) (third o)))
        (bq-list   (mapcar #'printable-bq-item (rest o)))
        (bq-list*  (build-appended (rest o) '*comma* '*dot-comma*))
        (bq-append (build-appended (rest o) '*comma-at* '*comma-at*))
        (t         (cons bq-op o))))
     ((and (constantp o)
           (readablep o)) o)
     (t (cons bq-op o)))))
;;;
;;; BQ-FLD-PRINTER
;;;
(deftype backquote-fld-form ()
  '(cons (eql backquote) (cons fld-form null)))

(def-cons-printer (cons (eql backquote) (cons fld-form null)) (stream list)
  (let* ((fform (second list))
         (object (fld-form-object fform))
         (comma-obj (comma-form-p object))
         (true-obj (if comma-obj (remove-comma object) object))
         (field (fld-form-field fform))
         (args (fld-form-args fform))
         (true-form (list* 'fld true-obj field args)))
    (if comma-obj 
        (princ "`." stream)
      (princ #\` stream))
    (prin1 true-form stream)))

;;;
;;; SYMBOL-PRINTER
;;;
(defun vertical-bar-symbol-name-p (name &optional (readtable *readtable*))
  (or (= 0 (length name))
      (let ((macrofn (get-macro-character (char name 0) readtable))
            (amacrofn (get-macro-character #\a readtable)))
        (or (and (not (eql macrofn amacrofn)) ; first is not a token char
                 (not (eql macrofn 'token-reader))
                 (not (eql macrofn #'token-reader)))
            (case (readtable-case readtable)
              (:upcase 
               (find-if (lambda (c)
                          (or (lower-case-p c)
                              (dot-or-terminator-p c)))
                        name))
              (:downcase
               (find-if 
                (lambda (c)
                  (or (upper-case-p c)
                      (dot-or-terminator-p c)))
                name))
              (:invertcase nil)
              (:preserve nil))))))

(defun mixed-case-p (string)
  (loop with case
        for c across string
        if (upper-case-p c)
        do (if case
               (unless (eq case :upcase)
                 (return-from mixed-case-p t))
             (setf case :upcase))
        else if (lower-case-p c)
        do (if case
               (unless (eq case :downcase)
                 (return-from mixed-case-p t))
             (setf case :downcase))))

(defun print-escaped-name (name stream)
  (flet ((print-case (char stream)
           (princ (case *print-case*
                    (:upcase  (char-upcase char))
                    (:downcase (char-downcase char))
                    (:invert (if (upper-case-p char) (char-downcase char)
                               (char-upcase char)))
                    (:preserve char))
                  stream))) 
    (let ((barsp   (and *print-escape* (vertical-bar-symbol-name-p name))))
      (if barsp (princ #\| stream))
      (let ((char-printer (case (readtable-case *readtable*)
                            (:upcase
                             (lambda (c stream) (if (upper-case-p c) 
                                             (print-case c stream)
                                           (princ c stream))))
                            (:downcase 
                             (lambda (c stream) (if (lower-case-p c)
                                             (print-case c stream)
                                           (princ c stream))))
                            (:preserve #'princ)
                            (:invert (if (mixed-case-p name) #'princ
                                       (lambda (c stream)
                                         (let ((*print-case* :invert))
                                           (print-case c stream))))))))
        (map nil (lambda (c)
                   (if (char= c #\|) (princ #\\ stream))
                   (funcall char-printer c stream))
             name))
      (if barsp (princ #\| stream)))))

               
(defmethod print-b-object ((symbol symbol) &optional stream)
  (with-platform-printing-environment
      (let* ((name       (symbol-name symbol))
	     (packagep   (and *print-escape*
			      (not (eq (find-symbol name) symbol))))
	     (package    (if packagep (symbol-package symbol)))
	     (internalp  (and package
			      (eq :internal
				  (second (multiple-value-list (find-symbol name package)))))))
	(when packagep
	  (case package
	    (#.(find-package "KEYWORD") nil)
	    ((nil)                      (princ #\# stream))
	    (t                          (print-escaped-name (package-name package) stream))))
	
	(if packagep (princ #\: stream))
	(if internalp (princ #\: stream))
	
	(print-escaped-name name stream)
	symbol)))

#-:clisp
(port:allowing-redefinitions 
 (defmethod print-object ((s symbol) stream)
   (print-b-object s stream)))

(defmethod print-b-object ((symbol symbol) &optional stream)
  (with-platform-printing-environment
      (let* ((name       (symbol-name symbol))
	     (packagep   (and *print-escape*
			      (not (eq (find-symbol name) symbol))))
	     (package    (if packagep (symbol-package symbol)))
	     (internalp  (and package
			      (eq :internal
				  (second (multiple-value-list (find-symbol name package)))))))
	(when packagep
	  (case package
	    (#.(find-package "KEYWORD") nil)
	    ((nil)                      (princ #\# stream))
	    (t                          (print-escaped-name (package-name package) stream))))
	
	(if packagep (princ #\: stream))
	(if internalp (princ #\: stream))
	
	(print-escaped-name name stream)
	symbol)))
#+:clisp (set-pprint-dispatch 
          'symbol (lambda (stream o) (print-b-object o stream)) 99 +top-level-pprint-dispatch-table+)

;;;
;;; CONS PRINTER
;;;
(defvar *current-print-depth* nil)
(defun current-print-depth () (or *current-print-depth* *print-level*))
(defun new-print-depth (increasep)
  (if *current-print-depth* 
      (if increasep (1- *current-print-depth*) (1+ *current-print-depth*))))

(defun cons-printer (stream cons &optional (prefix "(") (suffix ")") 
                            (*current-print-depth* (new-print-depth t)))
  (with-platform-printing-environment
    (let ((*print-context* cons))
      (cond
       ((eql *current-print-depth* 0) (princ #\# stream))
       (t
        (pprint-logical-block (stream nil :prefix prefix :suffix suffix)
          (loop for iter = cons then (cdr iter)
		for length-limit = *print-length* then (if length-limit (1- length-limit))
		until (not (consp iter))
		when (eql length-limit 0)
		do (princ "..." stream) (loop-finish)
		do (prin1 (first iter) stream)
		when  (cdr iter)
		do (princ #\space stream)
                (pprint-newline-selectively :linear stream)
		when (not (listp (cdr iter)))
		do   (princ ". " stream)
                (prin1 (cdr iter) stream))))))))


;;; 
;;; VECTOR-PRINTER
;;;
(defmethod print-b-object ((vector vector) &optional stream)
  (if (stringp vector)
      (if *print-readably*
          (with-standard-io-syntax
            (loop initially (princ #\" stream)
                  for c across vector
                  if (char= c #\") 
                  do (princ #\\ stream) (princ #\" stream)
                  else do (princ c stream)
                  finally (princ #\" stream)))              
        (map nil (lambda (c) (princ c stream)) vector))
    (cons-printer stream (coerce vector 'list) "#(")))

#-:clisp
(port:allowing-redefinitions
  (defmethod print-object ((v vector) stream)
    (print-b-object v stream)))

;;;
;;; FLD FORM PRINTER
;;;
(defun print-name (o &optional (stream *standard-output*)) (prin1 o stream))
(defvar *printing-fld-form-object* nil)

(def-cons-printer fld (stream fform)
  (fld-form-printer stream fform))

(defun fld-form-printer (stream fform) 
  (let* ((*print-context* fform)
         (printing-object *printing-fld-form-object*)
         (object (fld-form-object fform))
         (field  (fld-form-field fform))
         (args   (fld-form-args fform))
         (pfield (cond
                  ((any-comma-form-p field)
                   field)
                   ((or (not (constantp field))
                       (not (readablep field)))
                   (list '*comma* 'quote field))
                  ((comma-field-p field)
                   (cons '*comma* field))
                  ((keywordp field) 
                   (let ((*print-case* :upcase)) (sym field)))
                  (t field))))

      (let ((*printing-fld-form-object* t)
            (*print-level* (if *print-level* (1+ *print-level*))))
        (if (comma-form-p object)
            (prin1 (cons '*comma-dot* (remove-comma object)) stream)
          (prin1 object stream)))
      (princ #\. stream)
      (let ((*bq-printer-level* (+ 2 *bq-printer-level*))
            (*printing-fld-form-object* nil))
        (cond 
         ((equal field ''*brace*)
          (cons-printer stream args "{" "}"))
         ((equal field ''*bracket*)
          (cons-printer stream args "[" "]"))
         ((or args 
              (and printing-object
                   (any-comma-form-p pfield)))
          (cons-printer stream (list* pfield args)))
         (t    (prin1 pfield stream))))))

#-:clisp
(port:allowing-redefinitions
  (defmethod print-object ((o cons) stream)
    (print-b-object o stream)))

#+:lispworks
(progn
  (defun all-cons-printers (stream cons)
    (print-object cons stream))
  (set-pprint-dispatch 'cons 'all-cons-printers))

(defun unreadable-constantp (x)
  (typep x '(not (or standard-object standard-class structure-object))))

(defun comma-field-p (x)
  "Returns T if the object X, appearing as a field in a field form must be preceded by a comma"
  (or (and (symbolp x) (not (keywordp x)))
       (and (not (atom x))
            (unreadable-constantp x))))

;;;
;;; BASE CONCEPT-PRINTER
;;; 
(defmethod print-object :around ((o concept) stream)
  (declare (ignore o stream))
  (let ((*print-case*  :downcase))
    (with-platform-printing-environment
      (call-next-method))))

(defmethod print-object ((o concept) stream)
  (with-platform-printing-environment
    (let* ((*print-depth* (1+ *print-depth*))
           (id            (object-name o)))
      (cond
       ((and *print-context* id)
        (pprint-logical-block (stream ())
          (prin1 id stream)))

       ;; top-level-printing:
       (t 
        (pprint-logical-block (stream () :prefix (if id "{" "") :suffix (if id "}" ""))
          (when id 
            (let ((*print-context* o)) 
              (prin1 id stream))
            #-clisp (pprint-newline-selectively :linear stream)
            (princ " :# " stream))
          (print-concept o stream)))))))

(defgeneric print-concept (o &optional stream)
  (:documentation "Prints the data structure representation of a concept using print-eval consistency")
  (:method :around ((o concept) &optional stream)
   (unless (concept-hashkey o) (princ #\# stream))
   (call-next-method))
  (:method ((o concept) &optional (stream *standard-output*))
   (base-print-concept o stream)))

(defun base-print-concept (o stream)
  (let* ((cclass         (class-of o))
         (first-slot     nil)
         (*print-case*  :downcase))
    (flet ((print-value (o stream)
             (prin1 (self-evalify o) stream)))
      (with-print-context o
        (pprint-logical-block (stream nil :prefix "[" :suffix "]")
          (prin1 cclass stream)
          (pprint-logical-block (stream nil)
            (loop for fld in (cclass-id-field-order cclass)
                  do             
                  (let* ((fldinfo (class-fieldinfo cclass fld))
                         (ls  (fieldinfo-lambda-switch fldinfo))
                         (sym (fieldinfo-symbol fldinfo))
                         (val (fld o sym)))
                    (if first-slot 
                        (pprint-newline-selectively :linear stream)
                      (setf first-slot t))
                    (cond 
                     ((and (member ls '(&optional &key)) ;; ignore slots which are         
                           (null val)            ;; optional, NIL and have default=NIL
                           (null (fieldinfo-default fldinfo))))
                   
                     ((or (null ls)
                          (eq ls '&optional))
                      (princ #\Space stream)
                      (print-value val stream))
                   
                     ((eq ls '&key)                     
                      (pprint-logical-block (stream nil)
                        (princ #\Space stream)
                        (let ((*print-case* :upcase)) (prin1 (key sym) stream))
                        (pprint-newline-selectively :linear stream)
                        (princ #\Space stream)
                        (print-value val stream)))
                   
                     ((member ls '(&rest &body))
                      (let ((printed-first nil))
                        (mapcar (lambda (o)
                                  (if printed-first
                                      (pprint-newline-selectively :linear stream)
                                    (setf printed-first t))                                   
                                  (princ #\Space stream)
                                  (print-value o stream))
                                val)))

                     (t (error "Invalid slot lambda switch ~S" ls)))))))
        o))))

;;;
;;; HELPERS for CONCEPT PRINTING:
;;;
(defun self-evalify (x)
  "Converts the form X to a form which when evaluated yields x"
  (cond
   ((typep x '(or cons vector))     (if (readablep x) (list 'quote x)
                                      (backquotify x)))
   ((constantp x)                   x)
   (t                               (list 'quote x))))
   

(defun backquotify (x)
  (labels ((quote-if-necessary (elt)
             (cond
              ((consp elt) (bqify elt))
              ((constantp elt)  elt)
              (t
               (list 'quote elt))))
           (bqify (rest)
             (cons (self-evalify (first rest))
                   (let ((rrest (rest rest)))
                     (cond
                      ((consp rrest) (quote-if-necessary rrest))
                      ((null rrest)  rrest)
                      (t             (list rrest)))))))
    (if (dotted-list-p x)
        (list* 'bq-list* (bqify x))
      (list* 'bq-list (bqify x)))))

(defun readablep (x)
  (and (typep x '(not (or standard-object standard-class structure-object)))
       (or (symbolp x)
           (and (typep x 'structure-object)
                (equalp x (ignore-errors (read-from-string (format nil "~S" x)))))
           (and (consp x) 
                (readablep (first x))
                (readablep (rest x)))
           (and (simple-vector-p x)
                (every #'readablep x))
           (constantp x))))

 

