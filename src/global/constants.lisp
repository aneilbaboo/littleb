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

;;; File: constants
;;; Description: Constants, vars & parameters used by the language.  
;;;              Some are user-accessible.

;;; $Id: constants.lisp,v 1.3 2007/10/25 03:24:24 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(defconstant +b-user-package+ (find-package "B-USER"))

(defconstant +cl-expander+ (handler-case (symbol-value '+cl-expander+)
                             (error (e) *macroexpand-hook*)))
;;; PRINTER VARS
(defparameter *print-context* nil) ; nil at top-level, otherwise, 

(defparameter *print-depth* 0)
(defconstant *default-print-level* 10)
(defun b-debugger-hook (c dh) 
  (declare (ignore dh))
  (let ((*print-level* 20))  (invoke-debugger c)))
(setf *debugger-hook* 'b-debugger-hook)
(defun littleb-version ()
  "Returns 3 values: major version, minor version, revision number"
  (values b-system:*b-major-version*
          b-system:*b-minor-version*
          b-system:*b-revision*))

(defmacro with-print-context (o &body body)
  (let ((obj (gensym "OBJ")))
    `(let* ((,obj ,o)
            (*print-context* (if ,obj (cons ,obj *print-context*))))
       ,@body)))

(defun print-context-p (&optional (o nil o-p) (immediate t))
  (cond
   (immediate (eq o (first *print-context*)))
   (o-p       (member o *print-context*))
   (t         *print-context*)))
  
;;; INFO VARS
(defvar *objects* nil) ;; a nested list of objects which have been created (may be refered to by (nth-object N))
(defparameter *name* nil)    ;; NIL or a NAME object
(defparameter *relevance* nil)

;;;; ;;;; ;; PRINTER
;;;; (defconstant +pprint-b-expr-dispatch-table+ (with-standard-io-syntax (copy-pprint-dispatch)))
;;;; (defmacro with-b-pprint-dispatch (&body body)
;;;;   `(let ((*print-pprint-dispatch* (copy-pprint-dispatch +pprint-b-expr-dispatch-table+)))
;;;;      ,@body))

;; KB VARS
(defconstant +objects+ (make-hash-table  ; a map of ids to objects
                        :size 10007 ; 10007 is a prime
                        :rehash-size 2.0 :test 'equalp))


(defmacro unique-object (key form)
  "Returns the unique object associated with KEY (evaluated).  If none exists,
FORM is evaluated and resulting value is associated with KEY.  The key is cleared 
on (RESET) or (INIT).  Useful for storing an object which must be used as an identity value."
  (let ((hk '#:hk))
    `(let* ((,hk (cons :user ,key)))
       (or (gethash ,hk +objects+)
           (setf (gethash ,hk +objects+) ,form)))))
#+:lispworks (editor:setup-indent 'unique-object 2 4 2)

(defconstant +names+ (make-hash-table  ; a map of objects to ids
                        :size  10007 ; 10007 is a prime
                        :rehash-size 2.0 :test 'equalp))

(defconstant +fieldinfos+ (make-hash-table))

(defparameter *kb-halt* nil)
(defparameter *kb-undo-stack* nil)
(defparameter *kb-command-queue* nil)

(defvar *kb-trace-new-objects* t)
(defvar *kb-trace-delete-objects* t)
(defvar *kb-trace-update-objects* nil)
(defvar *kb-trace-rules* nil)

(defparameter *debug-printing* nil)

;; TRACE VARS
(defvar *tracing* nil) ;; replaces *trace-depth*
(defvar *trace-stack-location* (list ()))
(defconstant *hidden-classes* (make-hash-table)) ;; traceability & visibility in (objects)


;;;; (defvar *inside-kb-transaction-form* nil)

;;;; (defmacro kb-transaction (&body body)
;;;;   "ensures that any changes to the kb made in body are completed before 
;;;; any rules are triggered."
;;;;   (declare (special *inside-kb-transaction-form*))
;;;;   (let ((kb-halted (gensym "KB-HALTED"))
;;;;         (val       (gensym "VAL")))
;;;;     (cond 
;;;;      (*inside-kb-transaction-form*
;;;;       `(progn ,@body))
;;;;      (t
;;;;       (let ((*inside-kb-transaction-form* t))
;;;;         (declare (special *inside-kb-transaction-form*))
;;;;         `(let* ((,kb-halted *kb-halt*)
;;;;                 (*kb-halt*  t)
;;;;                 (,val      (progn ,@body)))
;;;;            (declare (optimize (speed 3)))
;;;;            (unless ,kb-halted 
;;;;              (kb-run)
;;;;              (flush-trace))
;;;;            ,val))))))

(defmacro kb-transaction (&body body)
  "ensures that any changes to the kb made in body are completed before 
any rules are triggered."
  (let ((kb-halted (gensym "KB-HALTED"))
        (val       (gensym "VAL")))
    `(let* ((,kb-halted *kb-halt*)
            (*kb-halt*  t)
            (,val       (macrolet ((kb-transaction (&body b) `(progn ,@b)))
                          (progn ,@body))))
           (declare (optimize (speed 3)))
           (unless ,kb-halted 
             (kb-run)
             (flush-trace))
           ,val)))

(defun retrieve-trace-stack (&optional (loc *trace-stack-location*))
  (reverse (first loc)))

(defun clear-trace-stack ()
  (setf *trace-stack-location* (list (list))))

(defun trace-stack-has-items-p ()
  (first *trace-stack-location*))

(defun reset-state-vars ()
  (clear-trace-stack)
  (clrhash +objects+)
  (clrhash +names+))


(defun get-b-path (name &optional child)
  "Where name is :init-file :root :user-data :user-home"
  (let ((child       (or child #P""))
        (path        (ecase name
                         ;; file which is run when little b is initialized:
                         (:init-file            (get-b-path :root "init.lisp"))

                         ;; location of executable or source tree
                         (:root                 *b-root-directory*)
                         
                         ;; portable user home folder
                         (:user-home            (portable:user-documents-folder))

                         ;; suggested location of user-libraries
                         (:user-data       (get-b-path :user-home "littleb/")))))
    (when path (normalize-pathname (merge-pathnames child path)))))

;;; ----------------------------------------------
;;; - the 'missing' symbol ?
;;; -----------------------------------------------
(defconstant ? '?)

(declaim (inline missingp))
(defun missingp (o)
  (eq o ?))

(defun hidden-symbol (&rest args)
  (apply #'delim-sym #\' args))

(reset-state-vars)
