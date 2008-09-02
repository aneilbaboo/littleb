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

;;; File: initialize
;;; Description: 

;;; $Id: initialize.lisp,v 1.9 2008/09/02 14:58:11 amallavarapu Exp $

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

  (initialize-global-value)

  (initialize-debugger)
  
  ;; load init file
  (when load-init-file-p (load-init-file))
   
  (format *standard-output* "~&Ready."))

