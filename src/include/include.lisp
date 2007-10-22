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

;;; File: include

;;; Description: Defines the b include system - for dynamically loading files.
;;;              This subsystem is intended relieve the user of having to write
;;;              system definition files as in standard lisp programs, and enforces
;;;              a discipline for package naming by coupling package names to file 
;;;              locations.  It is intended to be used instead of Common LISP's provide/
;;;              require functions or system definition utilities.
;;;
;;;              A list of included files (inclusions) is maintained by the system, so that
;;;              files are loaded only once when needed.  In addition, inclusions may be 
;;;              reloaded (in the order they were originally loaded by the system).
;;;
;;;    The main macro, is INCLUDE: (INCLUDE PATHS EXPOSURE) 
;;;        PATHS is an unevaluated form which may be either an include-path designator
;;;              or a list of include path designators.
;;;              * During loading and compiling, each inclusion (a compiled or source file) indicated by 
;;;                these paths will be loaded (if it has not been already)
;;;              * During compiling, each inclusion may be optionally compiled before loading.
;;;        EXPOSURE should be one of (NIL :USE :EXPOSE) default = :USE
;;;              * NIL - indicates no special extra handling of the included packages
;;;              * :USE - indicates that the included packages should be used (as by USE-PACKAGE)
;;;              * :EXPOSE - indicates that the included packages should be exposed (as by EXPOSE-PACKAGE) 
;;;
;;; $Id: include.lisp,v 1.3 2007/10/22 18:50:37 amallavarapu Exp $
;;;
(in-package b)

(defvar *include-requirements* (make-hash-table :test #'equalp)
  "A map from INCLUDE-PATH objects to files which have been included by it.")

(defvar *include-force* :changed
  "Allowed values: (NIL :changed T). 
           NIL         - only do operation if include-path has not been operated on
           :changed        - only perform operation if a new source file is present (different signature).
           T           - always perform operation")


(defvar *active-include-paths* ()
  "A list of include-paths which are currently waiting to complete loading
- used to detect circular includes.")

(defvar *include-verbose* t)

(defvar *include-suppress-modify-package-warning* nil)

;;; for maintaining a record of all ipaths loaded during the current invokation of INCLUDE
(def-binding-environment *current-loaded-ipaths* (make-hash-table :test #'equalp))

(defun currently-loaded-p (ipath)
  (gethash (include-path ipath) *current-loaded-ipaths*))

(defun (setf currently-loaded-p) (value ipath)
  (setf (gethash (include-path ipath) *current-loaded-ipaths*) value))

(defmacro with-include-load-unit (&body body)
  `(with-binding-environment (*current-loaded-ipaths* *signature-cache* *dependency-cache*)
     ,@body))
;;;;
;;;; INCLUDE LOADER IMPLEMENTATION:
;;;;
(defmacro include (include-paths &optional (exposure :use) 
                                 &key (force '*include-force*)
                                 (verbose '*include-verbose*)
                                 (modify nil))
  `(eval-when (:load-toplevel :execute)
     (include-dynamic ',include-paths ',exposure :force ,force :verbose ,verbose :modify ',modify)))

(defun include-dynamic (include-paths &optional 
                                      (exposure :use)
                                      &key
                                      (force *include-force*)
                                      (verbose *include-verbose*)
                                      (modify *include-suppress-modify-package-warning*))
  (when (find +b-user-package+ (package-use-list #.(find-package "CL-USER")))
    (assert nil () "BUG: CL-USER USES B-USER - (unuse-package :B-USER :CL-USER) AND TRY AGAIN."))
  (let ((*include-verbose*     verbose)
        (*include-suppress-modify-package-warning* modify))
    (with-include-load-unit
     (dolist (include-path (ensure-list include-paths))
       (let ((ipath (include-path include-path)))
         (with-edit-and-retry-restart (ipath :restart-name create)
           (load-single-include-path ipath exposure force)))))))

    
(defmacro with-include-edit-restart (include-path &body body)
  `(restart-case (progn ,@body)
     (edit () :report (lambda (s) (format s "Edit ~A - and abort." ,include-path))
           (prompt-for-yes-or-no "Inside with-include-edit-restart *EDIT-HOOK*=~S, IPATH=~S" *edit-hook* ,include-path)
           (edit ,include-path)
           (invoke-restart 'abort))))

(defvar *kb-size* 0)
(defun kb-touched () (decf *kb-size*))
(defun load-single-include-path (include-path exposure force)
  (let+ ((ipath               (include-path include-path))
         ((file sig lib type) (include-path-effective-file ipath))
         (src                 (include-path-source-file ipath))
         (ipath-pkg           (include-path-package ipath t)) ; ensure pkg created
         (iipath              (compute-current-include-path nil))
         (iipath-pkg          (include-path *package*)))
    (declare (ignorable lib))
    (setf *kb-size* (hash-table-count +objects+))
    ;; preliminary sanity checks:
    (check-ipath-circular-dependency ipath) ;circular dependency check
    (check-ipath-exists ipath file) ; file must
    (when src  ; check exposure is correct for current 
      (check-ipath-exposure ipath ipath-pkg exposure iipath iipath-pkg))
    
    ;; expose this package in the current package
    (ensure-package-exposure ipath-pkg exposure)
    
    (ensure-include-path-load ipath file sig type force)
    (if (> (hash-table-count +objects+) *kb-size*) 
        (reload-on-reset ipath))))

(defun ensure-package-exposure (pkg exposure)
  "Used to implement the appropriate EXPOSURE (:EXPOSE :USE or NIL) of PKG in the current package"
  (ecase exposure
    (:expose     (let ((pkg (strict-find-package pkg)))
                   (remove-package-conflicts *package* pkg)
                   (expose-package pkg)))
    (:use        (let ((pkg (strict-find-package pkg)))
                   (remove-package-conflicts *package* pkg)
                   (use-package pkg)))
    ((nil)   nil)))

(defun record-include-path-requirement (iipath ipath exposure)
  "records that ipath is loaded by iipath"
  (unless (equalp iipath ipath)
    (pushnew (list ipath exposure) (gethash iipath *include-requirements*) :test #'equalp :key #'first)))

(defun include-path-requirements (iipath)
  "returns all include-paths which have been loaded by iipath"
  (error "delete calling fn")
  (gethash (include-path iipath) *include-requirements*))
       
(defun clear-include-path-requirements (ipath)
  (remhash ipath *include-requirements*))


(defun ensure-include-path-load (ipath file file-sig type &optional (force *include-force*))
  "Returns T if ipath needed to be loaded"
  (let* ((ipath        (include-path ipath))
         (load-required-p          (and file
                                        (not (currently-loaded-p ipath))
                                        (case force
                                          ((nil) (null (include-path-current-signature ipath)))
                                          (:changed  (not (equalp file-sig (include-path-current-signature ipath))))
                                                         
                                          (t     t))))
         (mode                     (if (eq type :source) "SOURCE" "BINARY")))

    (package-mark-item-clearable (include-path-package-name ipath) ipath)

    (when load-required-p
      ;(clear-include-path-requirements ipath)
      
      (let ((*active-include-paths* (cons ipath *active-include-paths*)))
        (when *include-verbose*
          (format t "~&~60,,,'.<; ~{  ~1*~}> ~A ~; ~A~>~%" 
                  (rest *active-include-paths*) ipath mode))

        (let ((*package*   +b-user-package+))
          (platform-load-file file type))

        (setf (currently-loaded-p ipath) t)

        (setf (include-path-current-signature ipath) 
              file-sig))

      ;; return T if file was loaded
      t)))

#-:clisp
(defun platform-load-file (file type)
  (declare (ignorable type))
  (load file :verbose nil))

#+:clisp
(defun platform-load-file (file type)
  ;; this horrible kludge is necessary because the little b readtable
  ;; interferes with CLISP .FAS files (!!!), which are composed of S-expressions
  ;; read using what seems to be a modified version of the current readtable.
  ;; The little b readtable defines the alpha chars as macro characters, and this
  ;; appears to cause difficulties.  Don't know why. 
  ;; This code checks whether the current file being loaded is binary; if so,
  ;; it swaps the substitute readtable.
  ;; AM 9/06
  (let* ((binaryp (eq type :binary))
         (subst-readtable-p (not (eq *readtable* *working-readtable*)))
         (new-readtable (cond 
                         ((and binaryp subst-readtable-p) nil)
                         (binaryp (copy-readtable +b-standard-tokens-readtable+))
                         (subst-readtable-p *working-readtable*))))
    (cond
     (new-readtable (let ((*readtable* new-readtable)) (load file :verbose nil)))
     (t             (load file :verbose nil)))))

;;;
;;; SANITY CHECKS:
;;;
(defun check-ipath-circular-dependency (ipath)
  (whenit (position ipath *active-include-paths* :test #'equalp)
    (error "Circular INCLUDE dependency: ~A~{ <= ~A~} <= ~A" 
           ipath #1=(subseq *active-include-paths* 0 it) ipath)))

(defun check-ipath-exists (ipath &optional
                                 (file (include-path-effective-file ipath)))
  (unless file
    (error "Attempt to include ~A, but file ~A doesn't exist." ipath 
           (include-path-source-file ipath))))

(defun check-ipath-exposure (ipath  ; the include-path
                             ipath-pkg ; package ipath (in form of include-path object)
                             exposure
                             iipath  ; the including ipath
                             iipath-pkg) ; package name of including ipath (in form of ipath obj)
  "Checks that IPATH is exposed appropriately in the including path, IIPATH."
  (let* ((ipath          (include-path-spec ipath))
         (ipath-pkg      (include-path-spec ipath-pkg))
         (iipath         (include-path-spec iipath))
         (iipath-pkg     (include-path-spec iipath-pkg)))
    (flet ((file-modular-p () (equalp ipath ipath-pkg))
           (same-pkg-p ()     (equalp iipath-pkg ipath-pkg))
;;;;            (ancestor-p ()     (and (equalp iipath-pkg ipath-pkg) 
;;;;                                    (include-path-ancestor-p ipath iipath)))
           (suppress-p ()     (or (eq *include-suppress-modify-package-warning* t)
                                  (equalp (include-path-spec *include-suppress-modify-package-warning*) 
                                          ipath-pkg))))
      (unless (or (file-modular-p)
                  (same-pkg-p)
                  (suppress-p))
        (warn "Package ~A modified by (INCLUDE ~A~@[ ~S~]).  ~%~
           To suppress this warning, use (INCLUDE ~A~@[ ~S~]~@[ :MODIFY ~A~]).~%~
           (Warning generated in ~A)."
              ipath-pkg ipath exposure 
              ipath (unless (same-pkg-p) exposure)
              (unless (same-pkg-p) ipath-pkg)
              (or iipath "Listener"))))))

(defun include-funcall (include-path function &rest args)
  (let* ((ipath (include-path include-path)))
    (include-dynamic ipath nil)
    (let* ((fn-str (mkstr function))
           (pkg    (include-path-package ipath t))
           (fn-symbol (find-symbol fn-str pkg)))
      (unless (and fn-symbol (fboundp fn-symbol)) ()
        (error "Attempt to call undefined function ~A in ~A during INCLUDE-FUNCALL." 
               fn-str (package-name pkg)))
      (apply fn-symbol args))))
    


(defun include-symbol-reader (stream char char2)
  "Provides a macro reader for #/include-path:symbol - ensures that include-path is loaded, and gets the symbol"
  (declare (ignorable char char2))
  (let* ((symbolp     nil)
         (ipath       (with-output-to-string (ipath)
                        (loop for char = (read-char stream nil nil t)
                              for whitespace-char-p = (or (char= #\: char) (whitespace-char-p char))
                              until whitespace-char-p
                              do (princ char ipath)
                              finally (cond ((char= #\: char) (setf symbolp t))
                                            ((char= #\newline char) (unread-char char stream))))))
         (pkg         (include-path-package ipath t)))
    (cond
     ((include-path-source-file ipath) 
      (unless (or *read-suppress* *include-suppress*)
        (handler-case (include-dynamic ipath nil :modify (package-name pkg))
          (error (e) (b-reader-error stream "~A" e))))
      (cond
       (symbolp   (let ((*package* (if *read-suppress* *package* pkg)))
                    (read stream t nil t)))
       (t         pkg)))
     (t (b-reader-error stream "The include path ~A does not exist." ipath)))))

(set-dispatch-macro-character #\# #\/ 'include-symbol-reader +b-readtable+)
(set-dispatch-macro-character #\# #\/ 'include-symbol-reader +b-standard-tokens-readtable+)



;;;; (defun read-ipath-file-package-ipath (ipathfile)
;;;;   "Returns the package used by ipathfile by reading the first IN-PACKAGE form of the file"
;;;;   (let ((result     (with-dummy-b-package
;;;;                       (let+ (((ipath lib)     (include-path-from-pathname ipathfile))
;;;;                              (in-pkg-ipath    (include-path (read-in-package-name-from-file ipathfile))))
;;;;                         (if (or (equalp in-pkg-ipath ipath)
;;;;                                 (include-path-ancestor-p ipath in-pkg-ipath)
;;;;                                 (equalp in-pkg-ipath #.(include-path "B-USER")))
;;;;                             in-pkg-ipath
;;;;                           (list :error ipath lib in-pkg-ipath))))))
;;;;     (if (and (consp result) (eq (first result) :error))
;;;;         (destructuring-bind (err ipath lib in-pkg-ipath) result
;;;;           (declare (ignorable err)) ; always the symbol :ERROR
;;;;           (error "(IN-PACKAGE ~A) is invalid in file ~A (library ~A).  The package of this file must be either ~A, or a parent (~A)~
;;;;                   ~:[~; or some other ancestor~]."                
;;;;                  in-pkg-ipath ipath lib ipath 
;;;;                  #1=(include-path-parent ipath) 
;;;;                  (if (include-path-ancestors #1#) 0 1)))
;;;;       result)))
