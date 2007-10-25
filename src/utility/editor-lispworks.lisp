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

;;; File: editor-lispworks
;;; Description: alters the editor so it interprets {} and [] correctly.
;;;              doesn't yet handle complex dot expressions well, though.
;;;              E.g., [...].field is interpreted as 2 forms by the editor, but
;;;              as 1 form by the altered lisp reader.
;;;

;;; $Id: editor-lispworks.lisp,v 1.2 2007/10/25 20:13:02 amallavarapu Exp $

#+:lispworks
(in-package editor)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)

;(setf (editor:variable-value "CURRENT-PACKAGE" :global) nil) ;"B-USER")
  

(defparameter *b-syntax-table*
  (create-syntax-table  :string-escape #\\
                        :d-escape #\#
                        :double-comment #\#
                        :escape #\\
                        :comment #\;
                        :nested t
                        :end-comment #\newline
                        :second-comment #\|
                        :first-close-comment #\|
                        :second-close-comment #\#
                        :string #\" 
                        :close '(#\) #\] #\})
			:open '(#\( #\[ #\{)
                        :quote-char #\'
                        :backquote-char #\`
                        :comma-char #\,
                        :string-within-symbol #\|
                        :whitespace '(#\tab #\space
                                            #\page
                                            #\newline
                                            #\return)))

(setf (slot-value *b-syntax-table* 'operators) (slot-value *default-syntax-table* 'operators)
      *default-syntax-table* *b-syntax-table*
      (slot-value (get-mode-object "Lisp") 'syntax-table)
      *b-syntax-table*)


(defmacro with-temp-package (&body body)
  (let ((tmp '#:tmp))
    `(let* ((,tmp (make-package (gensym))))
       (unwind-protect
           (let ((*package* ,tmp))
             ,@body)
         (delete-package ,tmp)))))
       

(defcommand "B Edit" (p &optional name no-error-p (same-window-p t))
     "Edit the object referred to by form"
     "Edit the object referred to by form"
  (declare (ignorable name no-error-p same-window-p))
  (let* ((name         (or name (prompt-for-symbol p :prompt "Symbol: ")))
         (buffer-path  (buffer-pathname (current-buffer)))    
         (buffer-ipath (b:include-path-from-pathname buffer-path :errorp nil))
         (name-ipath   (ignore-errors
                         (cond
                          (buffer-ipath (let ((*package* (b:include-path-package buffer-ipath t)))
                                          (mutils:with-load-path buffer-path
                                            (b:include-path name))))
                          (t            (b:include-path name)))))
         (name-src     (when name-ipath
                         (mutils:whenit (ignore-errors (b:include-path-source-file name-ipath))
                           (probe-file mutils:it)))))
    (when name-src
      (ignore-errors 
        (b:include-path-package name-ipath t)))
    (if name-src (b:edit name-src)
      (find-source-command p name no-error-p same-window-p))))
    

(defun infer-current-package ()
  (or (ignore-errors (package-name 
                      (buffer-package-to-use (current-point))))
      *package*))


(defcommand "B Evaluate Buffer" (p)
     "Evaluates a buffer, creating the package, if necessary."
     "Evaluates a buffer, creating the package, if necessary."
  (declare (ignorable p))
  (let* ((b        (current-buffer))
         (b-path   (buffer-pathname b)))
    (mutils:with-load-path b-path
      (let* ((ipath    (if b-path (b:include-path-from-pathname b-path :errorp nil)))
             (src      (if ipath (probe-file (b:include-path-source-file ipath)))))
        (intern (b:include-path-spec ipath) "B-USER")
        (if src          
          (b:include-path-package ipath t))
 
        (editor:with-point ((end (buffer-point b)))
          (region-eval b (buffer-start (buffer-point b)) (buffer-end end)
                   :after-function
                   (lambda (&rest args) (declare (ignorable args))
                     (when src
                         (setf (b::include-path-current-signature ipath) 
                               (if (buffer-modified b) (list (cons (b:include-path-spec ipath)
                                                                   (get-universal-time)))
                                 (b::include-path-source-signature ipath)))))))))))

(defun buffer-editor-pane (b)
  (let* ((windows (buffer-windows b))
        (window  (or (find (current-window) windows) (first windows))))
    (window-text-pane window)))

(defcommand "B Evaluate Region" (p) 
     "Evaluates a region of a buffer, creating the package if necessary."
     "Evaluates a region of a buffer, creating the package if necessary."
 (let* ((b        (current-buffer))
        (b-path   (buffer-pathname b))
        (pane     (buffer-editor-pane b)))
    (mutils:with-load-path b-path
      (let* ((pkg-name (find-package-name-for-point b :point p))
             (ipath    (b:include-path pkg-name))
             (lib      (b:find-library (b:include-path-library-name ipath)))
             (src      (if lib (probe-file (b:include-path-source-file ipath)))))
        (intern (b:include-path-spec ipath) "B-USER")
        (if src
            (b:include-path-package ipath t))
        (if (capi:editor-pane-selected-text pane)
            (evaluate-region-command  pane)
          (evaluate-defun-command pane))))))


;; 19 Aug 1994, AA - only display progress messages if we aren't in
;; the echo area.
(defcommand "B Complete Token" (p)
     "Complete the symbol or package before the point, taking into account relative package names"
     "Complete the symbol or package before the point, taking into account relative package names"
  (with-point ((insert-point (current-point) :before-insert))
    (clear-echo-area-if-not-current "Completing...")
      (multiple-value-bind (string resolved-string package ipath-macro-p)
          (little-b-symbol-string-at-point insert-point)
        (let ((pkg (if ipath-macro-p (make-package (gensym) :use ()) package))) ; hack to force complete-symbol-1 to 
          (unwind-protect                                                         ; ensure that full pkg name is always displayed
              (multiple-value-bind (insert replace-length status)                 ; when #/ syntax is used
                  (complete-symbol-1 resolved-string
                                     :package pkg
                                     :predicate (if p 
                                                    #'(lambda (x)
                                                        (or (fboundp x)
                                                            (boundp x)))
                                                  nil))
                (if insert
                    ;; MW - 28/5/92 Using same method as in Replace String determine if the symbol
                    ;; being completed is being typed in lowercase, uppercase, or capitalized
                    (let ((index (position-if 'alpha-char-p string)))
                      (when (and index (upper-case-p (aref string index)))
                        (setf insert (if (let ((char (and (> (length string) (1+ index)) (aref string (1+ index)))))
                                           (and char (upper-case-p char)))
                                         (string-upcase insert)
                                       (string-capitalize insert))))
                      (replace-a-string insert-point
                                        (- (length string)); replace-length)
                                        insert))
                  (unless replace-length
                    (editor-error "No completions for ~S" string)))
                (clear-echo-area-if-not-current
                 (cond ((eq status :complete-but-not-unique) "Completed, but not unique")
                       ((eq status :not-unique) "Completion not unique")
                       (t "Finished completing"))))
            (if ipath-macro-p (delete-package pkg)))))))

(defun internal-symbol-difference (new old)
  (cond
   ((= (length new) (length old))
    (loop for i from 0 below (length new)
          unless (eq (svref new i) (svref old i))
          collect (svref new i)))
   (t (set-difference (coerce new 'list) (coerce old 'list)))))
      
(defmacro uninterning-new-symbols ((&optional (pkg *package*)) &body body)
  "Works - as long as forms in body do not intern AND unintern symbols"
  (let ((old-symbols '#:old-symbols)
        (s          '#:s)
        (package    '#:package)
        (pkg-eval   '#:pkg-eval))
    `(let* ((,pkg-eval    ,pkg)
            (,package     (if (packagep ,pkg-eval) ,pkg-eval (find-package ,pkg-eval)))
            (,old-symbols (copy-seq (system:package-internal-symbols ,package))))
       (unwind-protect (progn ,@body)
         (dolist (,s (internal-symbol-difference (system:package-internal-symbols ,package) ,old-symbols))
           (unintern ,s ,package))))))  

(defcommand  "B Insert Space and Show Arglist" (p)
     "Display the argument list in the echo area a while after inserting Space to the right of the function."
     "Display the argument list of functions and concept ctors."

  (ignore-errors
    (self-insert-command p #\Space)
    (with-point ((temp1 (current-point))
                 (temp2 (current-point)))
      (when (form-offset temp1 -1)
        (let ((*package* (or (buffer-package-to-use (current-buffer))
                             *package*))) ;; this fixes the bug which only showed doc for CL-USER symbols
          (when-let (x (ignore-errors (read-current-form temp1 temp2 :trim '(#\space))))
            (let* ((prev-char (read-prev-char temp1))
                   (window (current-window))
                   (interface (capi:top-level-interface (window-text-pane window)))
                   (symbol (find-function-or-ctor-for-arglist x)))
              (cond 
               ((b:fld-form-p x)
                (show-field-info x interface window))
               ((eql prev-char #\[)
                (when (and symbol (symbolp symbol))
                  (show-ctor-info symbol interface window)))
               ((b::object-form-p x) 
                (show-fields (b::object-form-object x) interface window))
               ((when (fboundp symbol)
                  (show-arglist symbol interface window)))
               ((and (consp x) (member (first x) '(function quote)))
                (when (fboundp (second x))
                  (show-arglist (second x) interface window)))))))))))

(defcommand "B Insert Dot and Show Fields" (p)
     "Display the fields of the object in the echo area a while after inserting Space to the right of the function."
     "Display the fields of an object."  
  (ignore-errors
    (self-insert-command p #\.)
    (with-point ((temp1 (current-point))
                 (temp2 (current-point))
                 (temp3 (current-point)))
      (form-offset temp1 -1) 
      (form-offset temp2 -2)
      (let* ((*package* (or (buffer-package-to-use (current-buffer))
                            *package*)) ;; this fixes the bug which only showed doc for CL-USER symbols
             (x (or (read-current-form temp1 temp3 :trim '(#\.))
                    (read-current-form temp2 temp3 :trim '(#\.))))
             (window (current-window))
             (interface (capi:top-level-interface (window-text-pane window))))
        (show-fields x interface window)))))
             
       
(defun show-fields (x interface window)
  (ignore-errors
    (cond
     ((b::object-form-p x) (show-fields (b::object-form-object x) interface window))
     ((symbolp x)          (when-let (val (ignore-errors (eval x)))
                             (if val
                                 (show-delayed-message interface window
                                                       (documentation val :fields-help))
                               (show-delayed-message interface window (documentation val :fields-help)))))
     ((b:fld-form-p x)     (show-delayed-message
                            interface window 
                            (documentation (compute-form-class x)
                                           :fields-help))))))

(defun compute-form-class (x)
  (cond
   ((b:fld-form-p x) (let* ((obj-class (compute-form-class (b:fld-form-object x)))
                            (fld-class (unless (eq obj-class #.(class-of nil))
                                         (ignore-errors 
                                           (find-class 
                                            (b:fld (b:fld obj-class :_fieldinfo (b:fld-form-field x)) 
                                                   :type))))))
                       (if (eq fld-class #.(find-class t)) nil
                         fld-class)))
   ((symbolp x)    (class-of (eval x)))))

(defun show-field-info (x interface window)
  (let* ((obj        (b:fld-form-object x))
         (field      (b:fld-form-field x))
         (doc-type   (cons :field-help field))
         (doc        (or (ignore-errors 
                           (when (symbolp obj)
                             (documentation (eval obj) doc-type)))
                         (ignore-errors (documentation (compute-form-class obj) doc-type)))))
    (show-delayed-message interface window doc)))


(defun show-ctor-info (symbol interface editor-window)
  (let* ((doc    (or (documentation symbol :ctor-help)
                     (ignore-errors (documentation (eval symbol) :ctor-help)))))
    (if doc
        (show-delayed-message interface editor-window doc)
      (show-delayed-message interface editor-window "No help info for [~A ...]." symbol))))


(defun read-prev-char (point)
  (let ((str (string-near-point point -1)))
    (unless (zerop (length str))
      (char str 0))))

(defun read-current-form (temp1 temp2 &key trim no-intern)
  "READS the FORM between points TEMP1 and TEMP2, trimming the char-bag TRIM.
   NO-INTERN directs the function to avoid interning the symbols (if the form is a symbol)."   
  (let ((str (string-right-trim trim (points-to-string temp1 temp2))))
    (cond
     ((zerop (length str)) nil)
     (no-intern            (let ((form (with-temp-package (read-from-string str))))
                             (cond 
                              ;; if a symbol was read, but it doesn't exist in the current package...
                              ((and (symbolp form)
                                    (not (find-symbol (symbol-name form))))
                               nil) ;; return NIL
                              (t (read-from-string str)))))
     (t                    (read-from-string str)))))

(defvar *show-delayed-message-timer*)
(defun show-delayed-message (interface editor-window  msg &rest args)
  (when msg
   (setq *arglist-timer* 
          (mp:make-timer 'capi:execute-with-interface interface 
                         (lambda (&rest args) 
                           (let ((*unknown-package-during-process-file* nil))
                             (apply #'process-character args)))
                         (list* 'message msg args)
                         editor-window))
    (mp:schedule-timer-relative *arglist-timer* *arglist-delay*)))


  
(defun find-function-or-ctor-for-arglist (x)
  (typecase x
    (symbol x)
    (list (unless (dotted-list-p x)
            (if (eq (length x) 1)
                (find-function-or-ctor-for-arglist (car x))
              (case (car x)
                ((quote function) (find-function-for-arglist (cdr x)))
                (setf (and (= (length x) 2)
                           (symbolp (second x))
                           x))))))))

(defun include-path-macro-token-p (string)
  (and (>= (length string) 2)
       (string= "#/" (subseq string 0 2))))

(defun little-b-symbol-string-at-point (insert-point)
  "Given an point, returns a SYMBOL-STRING, PACKAGE, IPATH-MACRO-P - IPATH-MACRO-P = bool indicating whether string starts with #/"
  (multiple-value-bind (string package)
      (symbol-string-at-point insert-point)
    (pathetic-resolve-little-b-partial-token string package)))

(defun pathetic-resolve-little-b-partial-token (string package)
  (let* ((ipath-macro-p (include-path-macro-token-p string))
         (string        (if ipath-macro-p (subseq string 2) string)))
    (multiple-value-bind (start length)
        (find-regexp-in-string "^[<>]+[^:.]*[:]?" string)
      (cond 
       ((and start length)
        (let* ((true-len (if (char= (char string (1- length)) #\:) (1- length) length))
               (newstr   (concatenate 'string
                                      (b:include-path-spec (b:include-path (subseq string 0 true-len) package))
                                      (subseq string true-len))))
          (values string newstr package ipath-macro-p)))

       ((and (find #\. string :test #'char=)
             (not (some #'upper-case-p string)))
        (values (string-upcase string) string package ipath-macro-p))
       (t (values string string package ipath-macro-p))))))

;;;;;
;;;;; THIS REPLACES THE LISPWORKS VERSION
;;;;;      
(defvar *lispworks-find-package-name-for-point* #'find-package-name-for-point)
(let ((*packages-for-warn-on-redefinition* nil))
(defun find-package-name-for-point (buffer &rest args)
  (let ((*load-truename* (or (buffer-pathname buffer)
                             (path-from-stepper-buffer-name buffer))))
    (apply *lispworks-find-package-name-for-point* buffer args))))



(defun path-from-stepper-buffer-name (buffer)
  (ignore-errors 
    (let ((name (buffer-name buffer)))
      (when (string-lessp "Stepper Buffer " name)
        (pathname (string-trim '(#\space) (subseq name #.(length "Stepper Buffer"))))))))

(defun infer-current-ipath ()
 (ignore-errors (b:include-path 
                 (or *load-truename*
                     *compile-file-truename*
                     (buffer-pathname (current-buffer))                                   (infer-current-package)
                     *package*))))


(defun b::enable-b-key-bindings ()
  (let ((emulation  #+mac :mac #-mac :pc))
    (bind-key "B Complete Token" "Ctrl-." :global emulation)
    (bind-key "B Edit" "Control-y" :global emulation)    
    (bind-key "B Evaluate Buffer" "F6" :global emulation)
    (bind-key "B Evaluate Region" "F7" :global emulation)
    (bind-key "B Insert Space and Show Arglist" #\Space :mode "Lisp" ) 
    (bind-key "B Insert Space and Show Arglist" #\Space :mode "Execute")
    (bind-key "B Insert Dot and Show Fields" #\. :mode "Lisp" ) 
    (bind-key "B Insert Dot and Show Fields" #\. :mode "Execute")

    t))


(setf hcl:*default-package-use-list* '#.(list "CL" "B"))


(defun b-form-offset (point n &optional (form t) (depth 0) not-prefix)
  (flet ((call-next-advice (&rest args) (apply #'form-offset args)))
    (cond 
     ((zerop n) (call-next-advice point 0 form depth not-prefix))
     
     ((plusp n)
      (when-let (p (call-next-advice point 1 form depth not-prefix))
        (let ((next1 (string-near-point p 1))
              (next2 (string-near-point p -1)))
        (cond
         ((string= "." next1)
          (increment-point p 1)
          (b-form-offset p n form depth not-prefix))
         ((and (string= "(" next1) (string= "." next2))
          (b-form-offset p n form depth not-prefix))
         (t
          (b-form-offset p (1- n) form depth not-prefix))))))


     ((minusp n)
      (when-let (p (call-next-advice point -1 form depth not-prefix))
        (let ((next1 (string-near-point p 1))
              (next2 (string-near-point p -1)))
        (cond
         ((string= "." next1)
          (b-form-offset p n form depth not-prefix))
         ((and (string= "(" next1) (string= "." next2))
          (increment-point p -1)
          (b-form-offset p n form depth not-prefix))
         (t
          (b-form-offset p (1+ n) form depth not-prefix)))))))))

(defun string-near-point (p &optional (offset1 0) (offset2 0))
  (with-point ((p1 p)
               (p2 p))
    (increment-point p1 offset1)
    (increment-point p2 offset2)
    (cond
     ((> offset2 offset1) (points-to-string p1 p2))
     ((< offset2 offset1) (points-to-string p2 p1))
     (t ""))))

)
(unless (boundp '*arglist-delay*)
  (defvar *arglist-delay* 1)
  (defvar *arglist-timer* nil))

(unless (fboundp 'show-arglist)
  (defun show-arglist (function interface editor-window)
    (setq *arglist-timer* 
          (mp:make-timer 'capi:execute-with-interface interface 
                         'process-character 
                         (list 'function-arglist-command nil function)
                         editor-window))
    (mp:schedule-timer-relative *arglist-timer* *arglist-delay*)))

(unless (fboundp 'find-function-for-arglist)
  (defun find-function-for-arglist (x)
    (typecase x
      (symbol x)
      (list (unless (dotted-list-p x)
              (if (eq (length x) 1)
                  (find-function-for-arglist (car x))
                (case (car x)
                  ((quote function) (find-function-for-arglist (cdr x)))
                  (setf (and (= (length x) 2)
                             (symbolp (second x))
                             x)))))))))


(ignore-errors
  (mutils:whenit (capi:find-interface 'lw-tools:listener)
    (setf (slot-value (slot-value (slot-value mutils:it 'lw-tools::editor-pane) 'capi::buffer)
                      'editor::syntax-table)
          *b-syntax-table*)))
  

(setf dbg:*debug-print-level* 8)