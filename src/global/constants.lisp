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

;;; File: constants
;;; Description: Constants, vars & parameters used by the language.  
;;;              Some are user-accessible.

;;; $Id: constants.lisp,v 1.10 2008/09/02 20:23:08 amallavarapu Exp $
;;; $Name:  $

(in-package b)

(defconstant +b-user-package+ (find-package "B-USER"))

(defconstant +cl-expander+ (handler-case (symbol-value '+cl-expander+)
                             (error (e)
                               (declare (ignore e)) *macroexpand-hook*)))
;;; PRINTER VARS
(defparameter *print-context* nil) ; nil at top-level, otherwise, 

(defparameter *print-depth* 0)
(defconstant *default-print-level* 10)

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
(port:setup-indent 'unique-object 2 4 2)

(defconstant +names+ (make-hash-table  ; a map of objects to ids
                        :size  10007 ; 10007 is a prime
                        :rehash-size 2.0 :test 'equalp))

(defconstant +fieldinfos+ (make-hash-table))

(defparameter *kb-halt* nil)
(defparameter *kb-undo-stack* nil)
(defparameter *kb-command-queue* nil)

(defparameter *debug-printing* nil)

(defmacro kb-transaction (&body body)
  "ensures that any changes to the kb made in body are completed before 
any rules are triggered."
  (let ((val       (gensym "VAL")))
    `(let* ((,val       (macrolet ((kb-transaction (&body b) `(progn ,@b)))
                          (let ((*kb-halt* t)) ,@body))))
       (declare (optimize (speed 3)))
       (unless *kb-halt*
         (let ((*kb-halt* t))
           (kb-run)))
       ,val)))

(defun reset-state-vars ()
  (clrhash +objects+)
  (clrhash +names+))


(defun get-b-path (name &optional child)
  "Where name is :init-file :root :user-data :user-home"
  (let ((child       (or child #P""))
        (path        (ecase name
                         ;; file which is run when little b is initialized:
                         (:init-file            (or b-system:*b-init-file* (get-b-path :root "init.lisp")))

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
