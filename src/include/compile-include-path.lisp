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

;;; File: compile-include-path

;;; Description: - most of this compilation system is a workaround for the
;;;                     fact that COMPILE-FILE handles errors during the compilation 
;;;                     process.  This means that treating INCLUDE as a "load, compiling
;;;                     if necessary"-style operation is not possible (since there would
;;;                     be no way of stopping the compile/load process when errors occur).
;;;
;;;
(in-package b)

(def-binding-environment *current-compiled-ipaths* (make-hash-table :test #'equalp))
(defun compilingp () *current-compiled-ipaths*)
(defun currently-compiled-p (ipath)
  (assert (include-path-p ipath))
  (gethash ipath *current-compiled-ipaths*))
(defun (setf currently-compiled-p) (value ipath)  
  (assert (include-path-p ipath))
  (setf (gethash ipath *current-compiled-ipaths*) value))

(defmacro with-include-compilation-unit (&rest body)
  `(with-binding-environment (*current-compiled-ipaths* *current-loaded-ipaths* *signature-cache* *dependency-cache*)
     (#+clisp ext:without-package-lock #+clisp ("COMMON-LISP")
      #-clisp progn
      (with-include-load-unit 
        (with-compilation-unit ()
          ,@body)))))


(defun needs-compile-p (ipath force recursive)
  (let ((ipath (include-path ipath)))
    (and (not (currently-compiled-p ipath))
         (ecase force
           ((nil :changed)  (let* ((src      (include-path-source-file ipath))
                                   (src-sig  (include-path-source-signature ipath))
                                   (cpl      (probe-file (include-path-compiled-file ipath)))
                                   (src-time (file-write-date src)))
                              (or (not (and cpl
                                            (equalp src-time (signature-write-time src-sig))
                                            (equalp src-sig (include-path-compiled-signature ipath))))
                                  (and recursive 
                                       (some (lambda (dep-sig) (not (equalp (cdr dep-sig) 
                                                                            (signature-write-time
                                                                             (include-path-source-signature (car dep-sig))))))
                                             (rest src-sig))))))
           ((t)             (not (currently-compiled-p ipath)))))))


(defun compile-include-path (ipath
                             &key 
                             (recursive     t)
                             (compile-force *include-force*)
                             (load-force    *include-force*)
                             (compile-verbose *compile-verbose*)
                             (load-verbose    *include-verbose*))
   (with-include-compilation-unit
   (let ((*compile-verbose* compile-verbose)
         (*include-verbose* load-verbose)
         (ipath (include-path ipath))
         (*package* (include-path-package ipath t)))
     (cond
      ((needs-compile-p ipath compile-force recursive)
       (dolist (op (nreverse (compute-ipath-operations ipath)))
         (process-include-path-compile-operation op compile-force load-force))
       (setf (currently-compiled-p ipath) t))
      (t (setf (currently-compiled-p ipath) t))))))


      
                 

(defun compile-and-load-ipath (ipath compile-force load-force)
  "Returns T if file was actually compiled."
;  (with-edit-and-retry-restart (ipath)
  (let* ((src                 (include-path-source-file ipath))
         (dest                (include-path-compiled-file ipath)))
    (let+ (((output warnings failure)  
            (compile-file-if-necessary ipath src dest compile-force)))
      (cond 
       ((and output (not failure))
        (setf (include-path-compiled-signature ipath) (include-path-source-signature ipath)
              (include-path-current-signature ipath) nil))
       (failure (setf (include-path-compiled-signature ipath) 0)))
                  
      (let+ ((*package*              (include-path-package ipath))
             (*include-force*        load-force)
             ((file sig lib type)    (include-path-effective-file ipath)))
        (declare (ignorable lib))
          
        (ensure-include-path-load ipath
                                  file sig type)
        (values output warnings failure)))))

(defun compile-file-if-necessary (ipath src dest force)
  (flet ((compile-destination ()
           (unless (currently-compiled-p ipath)
             (ensure-directories-exist dest)
             (clear-include-path-source-signature ipath)
             (let ((*package* +b-user-package+)) (when (platform-compile-file src dest)
                                                   (setf (include-path-compiled-signature ipath)
                                                         (include-path-source-signature ipath))))
             (setf (currently-compiled-p ipath) t))))
    (when (needs-compile-p ipath force nil)
      (compile-destination))))


(defun process-include-path-compile-operation (op compile-force load-force)
 (case (first op)
   (:make-package     (find-or-create-b-package (second op)))
   (:use-package      (process-exposure-operation op #'use-package))
   (:expose-package   (process-exposure-operation op #'expose-package))
   (:include-declaration (process-include-declaration-operation op))
   (:compile-and-load (compile-and-load-ipath (second op) compile-force load-force))))

(defun process-exposure-operation (op fn)
  (let ((ipath-pkg (second op))
        (iipath-pkg (third op)))
    (funcall fn (find-or-create-b-package ipath-pkg)
             (find-or-create-b-package iipath-pkg))))

(defun process-include-declaration-operation (op)
  "simulates the read/eval process for an include-declaration operation - which is needed to generated
symbols in the correct package."
  (let* ((ipath     (second op))
         (pkg       (third op))
         (*package* (find-or-create-b-package pkg :error)))
    (with-load-ipath (ipath)
      (with-edit-and-retry-restart (ipath)
        (let ((key-args  (simulate-read-form-symbols-in-package (nthcdr 3 op) *package*)))
          (apply #'include-declaration-operation ; call include-declaration-operation
                 *package* key-args))))))

(defun simulate-read-form-symbols-in-package (form pkg)
  "Returns a form in which all uninterned symbols are replaced with the symbols in pkg"
  (flet ((convert-form (o)
           (typecase o
              (keyword o)
              (symbol  (if (null (symbol-package o))
                           (intern (symbol-name o) pkg)
                         o))
              (t       o))))
    (typecase form 
      (atom   (convert-form form))
      (t      (maptree #'convert-form form)))))

(defun collect-include-operation-forms (ipath &optional (file (include-path-source-file ipath)))
  (with-open-file-for-edit-and-retry (stream file :direction :input)
    (with-load-ipath (ipath file)
      (loop with pkg = (include-path-package-name ipath)
            with eof = '#:EOF
            with *include-suppress* = t
            with prev-form = nil
            for form = (ignore-errors (with-dummy-b-package (read stream nil eof)))
            until (eq form eof)
            if (include-operation-form-p form)
            do (if prev-form (check-previous-form-valid form prev-form))
            and collect form into include-forms
            else do (check-for-invalid-form ipath form)
            do (setf prev-form (or form prev-form))
            finally (return (values  include-forms pkg))))))

(defun check-for-invalid-form (ipath form)
  (flet ((err (msg &rest args)
           (error "Invalid form ~S in ~A use ~? instead." form ipath msg args)))
    (when (consp form)
      (case (first form)
        (use-package    (err "(INCLUDE-DECLARATION :USE-PACKAGES ...)"))
        (expose-package (err "(INCLUDE-DECLARATION :EXPOSE-PACKAGES ...)"))
        (export-symbol  (err "(INCLUDE-DECLARATION :EXPORT-SYMBOLS ...)"))
        (expose-symbol  (err "(INCLUDE-DECLARATION :EXPOSE-SYMBOLS ...)")))))) 
  
(defun check-previous-form-valid (form prev-form)
  (flet ((prune-form (f) 
           (if (consp f) (format nil "(~S ...)" (first f))
             (format nil "~S" f))))
    (unless (include-previous-form-valid-p form prev-form)
      (error "Encountered ~A after ~A." 
             (prune-form form) (prune-form prev-form)))))

(defun include-previous-form-valid-p (form prev-form)
  (cond
   ((in-package-form-p form)        (null prev-form))
;;;;    ((include-declaration-form-p form)  (or (in-package-form-p prev-form)
;;;;                                         (include-declaration-form-p prev-form)))
   ((include-header-form-p form)    (include-header-form-p prev-form))
   (t                               t)))

(defun compute-ipath-operations (include-path &optional exposure (including-ipath include-path) ops)
  (let ((ipath           (include-path include-path)))
    (with-edit-and-retry-restart (ipath)
      (let* ((ipath-file         (include-path-source-file ipath))
             (ipath-pkg          (include-path-package-name ipath))
             (iipath             (or (include-path including-ipath)
                                     (compute-current-include-path nil)))
             (iipath-pkg         (if iipath (include-path-package-name iipath)
                                   (package-name *package*))))
        
        (flet ((add-operation (op) (pushnew op ops :test #'equalp))
               (operation-exists-p (op) (find op ops :test #'equalp)))

          ;; preliminary sanity checks:
          (check-ipath-circular-dependency ipath) 
          (check-ipath-exists ipath ipath-file)
          (check-ipath-exposure ipath ipath-pkg exposure iipath iipath-pkg)
        
          (add-operation `(:make-package ,ipath-pkg))

          (case exposure
            (:expose (add-operation `(:expose-package ,ipath-pkg ,iipath-pkg)))
            (:use    (add-operation `(:use-package ,ipath-pkg ,iipath-pkg))))

          (when (and ipath-file
                     (not (operation-exists-p `(:compile-and-load ,ipath))))
            (let ((*active-include-paths* (cons ipath *active-include-paths*))
                  (operation-forms        (collect-include-operation-forms ipath)))
              (with-load-ipath (ipath ipath-file)
                (dolist (oform operation-forms)
                  (let ((op   (first oform))
                        (args (rest oform)))
                    (case op
                      (include 
                       (destructuring-bind (iform-paths &optional (exposure :use) 
                                                        &rest keyword-args) args
                         (declare (ignorable keyword-args))
                         (dolist (iform-path (ensure-list iform-paths))
                           (setf ops (compute-ipath-operations (include-path iform-path)
                                                               exposure ipath ops)))))
                      (include-declaration 
                       (add-operation `(:include-declaration ,ipath ,ipath-pkg ,@args))))))
                (add-operation `(:compile-and-load ,ipath))))))))
      ops))


;;;
;;; LISP FORM TEST PREDICATES:
;;;   IN-PACKAGE USE-PACKAGE '... etc.
;;;
(defun include-form-p (o)
  (and (consp o) (eq (first o) 'include)))

(defun in-package-form-p (o)
  (and (consp o)
       (eq (first o) 'in-package)
       (= (length o) 2)))
                                     
(defun include-header-form-p (o)
  (or (in-package-form-p o)
      (include-form-p o)
      (include-documentation-form-p o)
      (include-declaration-form-p o)))

(defun exposure-symbol-p (o)
  (case o ((:use :expose nil) t)))

(defun include-operation-form-p (o)
  (or (include-declaration-form-p o) 
      (include-form-p o)))


;;;
;;; LIBRARY COMPILATION
;;;

(defun compile-library (lib  &key
                             (compile-force *include-force*)
                             (load-force *include-force*)
                             (compile-verbose *compile-verbose*)
                             (load-verbose *load-verbose*))
  (let* ((lib               (library-name lib))
         (lib-path          (ensure-library lib t))
         (directives   (library-compile-directives lib))
         (config-file  (library-init-file lib-path)))
    (unless directives (error "Cannot compile library ~S: no compile-paths have been defined.  ~
                           Add (WHEN-COMPILING ...) directive to ~S."
                              lib config-file))
    (with-include-compilation-unit
     (with-edit-and-retry-restart (config-file :display-name "library configuration file (_config.lisp)")
       (with-load-path lib-path
         (dolist (directive directives)
           (when *compile-verbose*
             (format t "~&; **** COMPILE-LIBRARY-DIRECTIVE: ~S ****~%" directive))
           (cond 
            ((consp directive)
             (let ((*load-truename* lib-path))
               (case (first directive)
                 (:clear  (clear))
                 (:reload (reload-include-paths (rest directive)))
                 (t       (error "Invalid library compile directive ~S defined in ~S." directive config-file)))))

            ((eq directive :clear)
             (clear))
             
            (t
              (compile-include-path directive
                                   :compile-force compile-force
                                   :load-force load-force
                                   :compile-verbose compile-verbose
                                   :load-verbose load-verbose)))))))))

(defun delete-library-binaries (&optional (lib :all))
  (typecase lib
    ((eql :all)     (dolist (lib (find-all-libraries))
                      (delete-library-binaries lib)))
    (pathname       (assert (pathname-directory-p lib))
                    (delete-directory (library-bin-dir lib) t t))
    (t              (delete-library-binaries (find-library lib)))))


;;;;
;;;; COMPILE SUPPORT FOR DIFFERENT PLATFORMS:
;;;;
(defun platform-compile-file (src dest)
  #-:clisp (compile-file src :output-file dest)
  ;; special handling for clisp, since the clisp binaries require standard-io-syntax:
  #+:clisp (compile-file-with-standard-io-syntax src :output-file dest))

#+:clisp
(progn
(defun compile-file-with-standard-io-syntax (src &rest args 
                                                 &key (output-file (compile-file-pathname src)))
  "Translates little b code to standard lisp before compiling"
  (let ((std-file (make-pathname :type "tmp" :defaults src))
        (forms    (read-littleb-file src)))
    (unwind-protect
        (progn
          (write-standard-lisp-file forms std-file)
          (ext:execute "C:/bin/clisp/clisp.exe" 
                       (format nil "-c \"~A\" -o \"~S\""
                               src output-file)))
      (delete-file std-file))))
    
(defun read-littleb-file (file)
  (with-open-file (stream file :direction :input)
    (let ((*compile-file-truename* (pathname file))
          (*compile-file-pathname* *compile-file-truename*)
          (*load-truename* *compile-file-truename*)
          (*load-pathname* *compile-file-truename*))
      (read-forms stream))))
      
(defun write-standard-lisp-file (forms file)
  (with-standard-io-syntax
    (setf *print-pretty* t) ; use the pretty printer (override the print-object methods)
    (with-open-file (stream file :direction :output :if-does-not-exist :create)
      (write-forms forms stream))))

(defun write-forms (forms stream)
  (loop with *package* = *package* ;; introduce a binding
        for form in forms
        when (in-package-form-p form)
        do (eval form)
        do (prin1 form stream)))
        
(defun read-forms (stream)
  (loop with *package* = *package* ;; introduce a binding
        for form = (read stream nil #1='#:eof nil)
        until (eq form #1#)
        when (in-package-form-p form)
        do (eval form)
        collect form))
); #+:clisp
    
;;;; (defun compile-include-path (ipath &key (recursive t)
;;;;                                    (compile-verbose *compile-verbose*)
;;;;                                    (compile-force *include-force*)
;;;;                                    (load-verbose *include-verbose*)
;;;;                                    (load-force *include-force*))
;;;;   (with-include-compilation-unit 
;;;;    (with-signature-cache
;;;;      (let* ((ipath                   (include-path ipath))
;;;;             (src-sig                 (include-path-source-signature ipath))
;;;;             (*current-loaded-ipaths* (if (eq *current-loaded-ipaths* :top-level) ()
;;;;                                        *current-loaded-ipaths*))
;;;;             (dependencies            (include-path-dependencies ipath))
;;;;             (*compile-verbose*       compile-verbose)
;;;;             (*include-verbose*       load-verbose)
;;;;             (needs-compile-p         (needs-compile-p ipath compile-force))
;;;;             (src-path                (include-path-source-file ipath))
;;;;             (cpl-path                (include-path-compiled-file ipath)))

;;;;        ;; only if this file needs compiling - or recursive is T
;;;;        (when (or recursive needs-compile-p)

;;;;          (let ((*compile-package* (include-path-package-name ipath)))
;;;;            (with-temporary-b-package ()               

;;;;              (setf (currently-compiled-p ipath) t)

;;;;              ;; if recursive, compile dependencies
;;;;              (when recursive 
;;;;                (dolist (dep dependencies)
;;;;                  (compile-include-path dep :recursive recursive
;;;;                                        :compile-force compile-force
;;;;                                        :load-force load-force)))

;;;;              (when needs-compile-p
;;;;                ;; load any dependencies
;;;;                (dolist (dep dependencies)
;;;;                  (include-dynamic dep nil :force load-force))

;;;;                ;; finally, compile the file...
;;;;                (ensure-directories-exist cpl-path)
;;;;                (compile-file src-path
;;;;                              :output-file cpl-path)
;;;;                (setf (include-path-compiled-signature ipath)
;;;;                      (include-path-source-signature ipath))))
;;;;            t))))))
;;;;   
