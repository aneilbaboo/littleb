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

;;; File: initialize
;;; Description: 

;;; $Id: initialize.lisp,v 1.7 2007/12/06 22:25:20 amallavarapu Exp $

(in-package b)

(defun system-database-initialization ()
  (clear-signatures)
  (clear-assumptions)
  (kb-transaction 
    (kb-reset)
    (reset-state-vars))
  (when *include-verbose*
    (format t "~&; Database reset.")))

(defun initialize-readtable ()
  (setf *readtable* (copy-readtable +b-readtable+ *readtable*)
        *working-readtable* *readtable*
        ;*print-pprint-dispatch* (copy-pprint-dispatch +top-level-pprint-dispatch-table+)
        ))

(defun initialize-print-dispatch ()
  (setf *print-pretty* t ;; necessary for lispworks personal
        *print-pprint-dispatch* +top-level-pprint-dispatch-table+))

(defun initialize-syntax ()
  (initialize-readtable)
  (initialize-print-dispatch)
  (initialize-object-expanders))

(defun initialize-object-expanders ()
  (setf *object-expanders* nil)
  (add-object-expander 'cclass-expander most-negative-fixnum))

(defun initialize-debugger ()
  (setf *debugger-enabled* nil
        *debugger-hook* 'littleb-debugger-hook))

(defun init (&optional (load-init-file-p t))
  "Sets the little b environment to the initial state, clears definitions in temporary packages,
   and loads the initialization file."
  (let ((*include-verbose* nil)) (clear))

  (when *include-verbose*
    (format *standard-output* "~&Initializing system...~%"))
  
  (initialize-syntax)

  (system-database-initialization)

  (initialize-debugger)
  
  ;; load init file
  (when load-init-file-p (load-init-file))
   
  (format *standard-output* "~&Ready."))

