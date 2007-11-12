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

;;; File: clear.lisp
;;; Description: Package reset functionality.

;;; $Id $
(in-package b)

(defvar *package-clearable-items* (make-hash-table :test #'equalp)
  "A map from package names (strings) to lists of objects to be cleared.")
(defvar *clear-locked-packages* (let ((ht (make-hash-table :test #'equalp)))
                                  (mapc (lambda (p) (setf (gethash (package-name p) ht) t))
                                        (remove (find-package "B-USER") (list-all-packages)))
                                  ht)
  "A hash-table containing packages which must not be cleared.")
(defvar *reload-on-reset-list* nil
  "List used internally by reset - include paths which should be reloaded when RESET is called. ")

(defvar *reset-target* :B-USER
  "A package name designator (or a list) naming packages which should be cleared when reset is called.")

;;;
;;; RESET
;;;
(defun reset (&optional (target *reset-target*)) ;(defaults *reset-defaults*))
  "Resets the system - resets the database. Target packages are cleared and default files are reloaded.
Parameters:
TARGET - Either: Package specifier(s) indicating package(s) to be cleared (a list is also acceptable),
                   default is the value of *RESET-TARGET* which is :B-USER by default.
         Or      The symbol T indicating all packages should be cleared.
USAGE: assuming *RESET-TARGET* = :B-USER,
       (reset) - clears definitions made in the B-USER package.
       (reset t) - clears all definitions and reloads init file.
NOTE: The system may load files after RESET.  This is done to repopulate the database.  Files which affect
packages other than those in TARGET and which affect the database are reloaded.  The user may indicate that
a file modifies the database (or depends on information in the database) by calling (RELOAD-ON-RESET) inside
the file."
  (case target
    ((t)  
     (clear)
     (load-init-file))
    (t
     (let* ((pkgs (mapcar (lambda (pkg) (strict-find-package pkg)) (ensure-list target)))
            (use-lists (mapcar #'package-use-list pkgs))
            (names (mapcar #'package-name pkgs))
            (reload-list (reverse *reload-on-reset-list*)))
       (dolist (p pkgs) (clear-package p))
       (system-database-initialization)
       
       (load-init-file)
       (when (and reload-list *include-verbose*) (format t "~&; Reloading..."))
       (dolist (x reload-list)
         (if (pathnamep x) (load x)
           (unless (member (include-path-package-name x) names :test #'string-equal)                       
             (include-dynamic x nil :modify t))))
       (loop for pkg in pkgs for use-list in use-lists
             do (use-package use-list pkg)))))
  t)

(defun reload-on-reset (&optional ipath)
  "Causes a file (default=current ipath) to be reloaded when RESET is called."
  (let ((ipath (if (pathnamep ipath) ipath
                 (or (include-path ipath) 
                     (ignore-errors (compute-current-include-path nil))
                     *load-truename*))))
    (when ipath
      (pushnew ipath *reload-on-reset-list* :test #'equalp))
    t))


(defun load-init-file ()
  (let ((ifile (b:get-b-path :init-file))
        (*load-verbose* nil))
    (cond
     ((probe-file ifile)
      (when *include-verbose*
        (format t "~&; Loading initialization file ~A..." ifile))
      (load ifile))
     (*include-verbose*
      (format t "~&; Initialization file not found: ~A~%"
              (get-b-path :init-file))))))
      
  
;;;
;;; CLEAR
;;;
(defun clear ()
  "Clears the system: all included packages are emptied, all files marked as not loaded, all assumptions cleared.  Reloads files specified in defaults."
    (setf *reload-on-reset-list* nil)
    (clrhash *library-info*)
    (system-database-initialization) 
    (loop for k being the hash-keys of *package-clearable-items*
          do (clear-package k))
    (clear-package (find-package "B-USER"))
    (clear-assumptions)
    (clear-signatures)
    #+Lispworks (hcl:mark-and-sweep 3)
    (when *include-verbose*
      (format t "~&; Cleared ~A package~:P." 
              (hash-table-count *package-clearable-items*)))
   
    t)

;;;
;;; CLEAR-ITEM GENERIC FUNCTION
;;;
(defgeneric clear-item (x)
  (:documentation "Clears the item from the include system."))

(defmethod clear-item ((f function)) (funcall f))
;;;
;;; INCLUDE-PATH CLEARING
;;;
(defmethod clear-item ((ipath include-path))
  (setf (include-path-current-signature ipath) nil)
  (clear-include-path-requirements ipath))

;;;;
;;;; PACKAGE CLEARING
;;;;
(defun package-mark-item-clearable (pkg &optional x)
  (let* ((pkg      (strict-find-package pkg))
         (pkg-name (package-name pkg))
         (existing (gethash pkg-name *package-clearable-items*)))
    (setf (gethash pkg-name *package-clearable-items*)
          (if x (pushnew x existing :test #'equalp)
            existing))
    pkg))

(defun package-clearable-items (pkg)
  (let ((name (package-name (strict-find-package pkg))))
    (gethash name *package-clearable-items*)))

(defun clear-package (pkg)
  (let* ((used-by (union (package-used-by-list pkg)
                         (package-exposing-packages pkg)))
         (name    (package-name pkg)))
    (dolist (item (package-clearable-items pkg))
      (clear-item item))
    (setf (gethash (package-name (strict-find-package pkg))
                   *package-clearable-items*) nil)
    (destroy-symbols pkg)
    (unexpose-package (package-use-list pkg) pkg)
    (dolist (upkg used-by)
      (unexpose-package pkg upkg))
    (do-external-symbols (s pkg)
      (unexpose-symbol s t pkg))
    (do-symbols (s pkg)
      (unintern s pkg))

    ;; remove all ipaths that affect this package from the reload on reset list
    (setf *reload-on-reset-list* (delete name *reload-on-reset-list*
                                         :key #'include-path-package-name
                                         :test #'string=))

    (use-package '(cl b) pkg)))

(defun destroy-symbols (pkg)
  "Uninterns symbols in the current package, destroying the DSPECs if possible"
  (do-symbols (s pkg)
    (when (or (null pkg) (eq (symbol-package s) pkg))
      (when (globalp s) (clear-global s))
      (portable:destroy-symbol s :undefinep nil :forget-dspec-p t :uninternp nil))
    (unintern s pkg)))


;;;; REMOVE this
;;;; ;;;
;;;; ;;; CLEAR STACK
;;;; ;;;
;;;; (defun compute-clear-stack ()
;;;;   (loop  for v being the hash-values of *clearables*
;;;;          when v collect v into items
;;;;          if (include-path-p v) sum 1 into ipath
;;;;          else if (packagep v) sum 1 into package
;;;;          finally (return (values items ipath package))))
