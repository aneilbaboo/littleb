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

;;; File: expose
;;;
;;; Description: Extends the notion of Common LISP's EXPORT function,
;;;              The functions cause exposed symbols of one package to
;;;              become external symbols of another package.
;;;              
;;;              In addition, a package (A) may expose all the symbols of
;;;              another package (B).  In this second case, any time a 
;;;              new symbol is exposed in package B, it will also be exposed
;;;              in package A, and consequently any packages which expose A too.
;;;              

;;; $Id: expose.lisp,v 1.1 2007/09/25 17:54:11 amallavarapu Exp $
;;; $Name:  $
(in-package b)

(defvar +exposing-packages+ (make-hash-table :test #'eq)
  "A map from packages to packages which expose them")

(defconstant +exposed-by+ '#:exposed-by)

;;;
;;; EXPOSE-PACKAGE
;;;
(defun expose-package (packages-to-expose &optional (package *package*))
  (let ((package (strict-find-package package))
        result)
    (dolist (o (ensure-list packages-to-expose) result)
      (if (expose-single-package (strict-find-package o) package)
          (setf result t)))))
           
(defun expose-single-package (pkg-to-expose package)
  (declare (type package pkg-to-expose package))
  (unless (eq pkg-to-expose package)
    (let ((exposing-packages (gethash pkg-to-expose +exposing-packages+)))
      (unless (member package exposing-packages)
        (use-package pkg-to-expose package)
        (do-external-symbols (s pkg-to-expose)
          (expose-symbol s package))
        (setf (gethash pkg-to-expose +exposing-packages+)
              (push package exposing-packages))
        t))))

(defun package-exposed-symbols (pkg)
  (let ((symbols ()))
    (do-external-symbols (s pkg symbols)
      (when (eq (get s +exposed-by+) pkg)
        (push s symbols)))))
;;;
;;; UNEXPOSE-PACKAGE
;;;
(defun unexpose-package (packages-to-unexpose &optional (package *package*))
  (let ((package (strict-find-package package)))
    (dolist (o (ensure-list packages-to-unexpose))
      (unexpose-single-package (strict-find-package o) package))
    t))

(defun unexpose-single-package (epkg package)
  "unexposes epkg from package"
  (declare (type package epkg package))
  (let ((exposed-symbols (package-exposed-symbols epkg)))
    (dolist (s exposed-symbols)
      (unexpose-symbol s package))
    (setf (gethash epkg +exposing-packages+)
          (delete package (gethash epkg +exposing-packages+)))
    (unuse-package epkg package)))

;;;
;;; EXPOSE-SYMBOL
;;;
(defun expose-symbol (symbols-to-expose &optional (package *package*))
  (let ((result ())
        (package (strict-find-package package)))
    (flet ((expose-all (force)
             (dolist (o (ensure-list symbols-to-expose) result)
                    (whenit (expose-single-symbol (strict-find-symbol o) package force)
                      (push it result)))))
      (restart-case (expose-all nil)
        (continue () 
                  :test (lambda (c) (typep c 'expose-symbol-error))
                  :report (lambda (stream)
                            (format stream "Unexpose all conflicting symbols from their home packages."))
                  (expose-all t)))))
  t)


(defun external-symbol-conflict-error (external-symbol symbol package)
  "An error which reports that incoming EXTERNAL-SYMBOL conflicts with SYMBOL in PACKAGE."
  (let* ((sname         (symbol-name symbol))
         (esname        (symbol-name external-symbol))
         (s-pkg         (symbol-package symbol))
         (s-pkg-name    (package-name s-pkg))
         (es-pkg-name   (package-name (symbol-package external-symbol)))
         (pkg-name      (package-name package)))
    (restart-case (error "Attempting to import ~A:~A into ~A, which contains ~
                            conflicting symbol ~A:~:[:~;~]~A."
                         es-pkg-name esname
                         pkg-name
                         s-pkg-name (symbol-external-p symbol) sname)
      (continue () :report (lambda (s)
                             (format s "Replace the conflicting symbol in package ~A and all ~
                                        packages which use ~A." pkg-name pkg-name))
                (transitive-replace-internal-symbol symbol external-symbol package)))))

(defun replace-internal-symbol (internal-symbol new-symbol package)
  (warn "Replacing ~S in ~A with ~A:~A"
        internal-symbol
        (package-name package)
        (package-name (symbol-package new-symbol)) (symbol-name new-symbol))
  (unintern internal-symbol package)
  (import new-symbol package))

(defun transitive-replace-internal-symbol (internal-symbol new-symbol package)
  (dolist (upkg (nconc (package-transitive-used-by-list package) (list package)))
    (replace-internal-symbol internal-symbol new-symbol upkg)))


(defun safely-remove-conflicting-internal-symbols (s &optional (package *package*))
  (etypecase s
    (symbol (let+ ((sname                   (symbol-name s))
                   ((internal-symbol status) (find-symbol sname package)))
              (when internal-symbol
                (cond
                 ((and (not (eq s internal-symbol))
                       (or (fboundp internal-symbol)
                           (boundp internal-symbol)
                           (globalp internal-symbol)
                           (eq status :external)))
                  (external-symbol-conflict-error s internal-symbol package))

                 ((not (eq s internal-symbol))
                  (replace-internal-symbol internal-symbol s package))))))
    (cons   (safely-remove-conflicting-internal-symbols (pop s) package))
    (null   ())))

(defun remove-package-conflicts (package packages-to-use)
  "Removes any external symbols of package-to-use from package (default = current package)"
  (let ((packages-to-use (mapcar #'strict-find-package (ensure-list packages-to-use)))
        (package         (strict-find-package package)))
    (dolist (package-to-use packages-to-use)
      (do-external-symbols (s package-to-use)
        (safely-remove-conflicting-internal-symbols s package)))
    (dolist (used-by (package-used-by-list package))
      (remove-package-conflicts used-by packages-to-use))))

;;;; without restart - for debugging purposes
;;;; (defun expose-symbol (symbols-to-expose &optional (package *package*))
;;;;   (let ((result ())
;;;;         (package (strict-find-package package)))
;;;;     (flet ((expose-all (force)
;;;;              (dolist (o (ensure-list symbols-to-expose) result)
;;;;                     (whenit (expose-single-symbol (strict-find-symbol o) package force)
;;;;                       (push it result)))))
;;;;       (expose-all nil))))

(defun expose-single-symbol (s package &optional force)
  "Returns T if the symbol was newly exposed by this call"
  (declare (type symbol s)
           (type package package))
  (let ((exposing-pkgs (symbol-exposing-packages s)))
    (handler-case 
        (progn           
          (unintern-from-using-pkgs s package force)           
          (import s package)
          (export s package))
      (error (e) (expose-symbol-error s package e)))
    (setf (get s +exposed-by+) (pushnew package exposing-pkgs))
    ;; transmit this symbol to packages which expose this package.
    (dolist (p (package-exposing-packages package))
      (expose-single-symbol s p force))
    s))

(defun unexpose-symbol (symbols-to-unexpose &optional (unintern nil) packages)
  "Causes the symbol or symbols to be hidden in all PACKAGES (default = ALL the exposing packages)
The UNINTERN argument may be 
    NIL (do not unintern) 
    :IMPORTING (unintern in all importing packages, but not the home package) 
    or T (unintern from all packages)"
  (let ((unintern-fn (ecase unintern
                       ((nil)        (lambda (s p) (declare (ignorable s p)) ()))
                       ((t)          (lambda (s p) (unintern s p)))
                       (:importing   (lambda (s p) (unless (eq p (symbol-package s))
                                                       (unintern s p))))))
        (change      nil))
    (dolist (s (ensure-list symbols-to-unexpose) change)
      (when (unexpose-single-symbol s (or packages (symbol-exposing-packages s)) unintern-fn)
        (setf change t)))))

(defun unexpose-single-symbol (s packages unintern-fn)
  (loop with change = nil
        for p in (ensure-list packages)
        when (symbol-in-package-p s p)
        do (unexport s p)
           (funcall unintern-fn s p)
           (setf change t)
        finally (return change)))

(defun symbol-in-package-p (s p)
  (eq (symbol-package (find-symbol (symbol-name s) p)) p))

            

(defun package-transitive-used-by-list (p)
  (let ((encountered ()))
    (labels ((find-all (p)
               (loop for upkg in (package-used-by-list p)
                     unless (member upkg encountered)
                     do (push upkg encountered)
                     (find-all upkg))))
      (find-all p)
      encountered)))
    
(defun unintern-from-using-pkgs (s package &optional (force t))
  (let ((sname (symbol-name s)))
    (dolist (pkg (cons package (package-transitive-used-by-list package)))
      (whenit (find-symbol sname pkg) 
        (cond 
         ((and force 
               (not (eql (symbol-package it)
                         (symbol-package s))))
          (unexport it pkg)
          (unintern it pkg))
         (t (safely-remove-conflicting-internal-symbols s pkg)))))))
  
(defun symbol-exposing-packages (o)
  (get (strict-find-symbol o) +exposed-by+))

(defun symbol-exposed-p (o &optional p)
  (if p (member (symbol-exposing-packages o) p)
    (symbol-exposing-packages o)))

(defun package-exposed-p (p &optional (package *package*))
  (member package (package-exposing-packages p)))

(defun package-exposing-packages (&optional (package *package*))
  (values (gethash (find-package (strict-find-package package)) +exposing-packages+)))

(defun strict-find-package (o)
   (etypecase o
    (symbol       (strict-find-package (symbol-name o)))
    (string       (ifit (find-package o) it
                    (error "Package ~S does not exist." o)))
    (include-path (strict-find-package (include-path-spec o)))
    (package      o)))
 
(defun strict-find-symbol (o)
  (typecase o
    (string (ifit (find-symbol o) it))
    (symbol o)))

;;;
;;; ERRORS
;;;
(define-condition expose-symbol-error (b-error)
  ((symbol :initarg :symbol :reader expose-symbol-error-symbol)
   (package :initarg :package :reader expose-symbol-error-package)
   (error :initarg :error :reader expose-symbol-error-error))
  (:default-initargs :format-string "Error while exposing symbol ~S in ~S: ~A"))

(defun expose-symbol-error (symbol package error)
  (error 'expose-symbol-error :symbol symbol :package package :error error))

(defmethod print-object ((e expose-symbol-error) stream)
  (with-slots (format-arguments symbol package error) e
    (setf format-arguments (list symbol package error))
    (call-next-method)))
