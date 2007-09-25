;;; This is asdf+ some useful extensions to ASDF (Another System Definition Facility).  
;;; $Revision: 1.1 $

;;; You may load this file instead of ASDF.lisp, provided that either ASDF.lisp has
;;; already been loaded or is present in the same folder as this file.

;;;
;;; Feedback, bug reports, and patches are all welcome: please mail to
;;; <aneilbaboo@gmail.com>.  
;;;
;;; Copyright (c) 2001-2003 Aneil Mallavarapu
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;
;;; This file contains some handy extensions to ASDF.
;;;
;;;  LOAD-SYSTEM: syntactic sugar for (asdf:oos 'asdf:load-op __)
;;;  FILES-IN-SYSTEM: analogous to same fn name in MK-DEFSYSTEM, returns all files
;;;  SYSDEF-CENTRAL-REGISTRY-SUBSEARCH: enables automatic search of central registry
;;;    subdirs, based on name of system. 
;;;           E.g., (asdf:load-system 'mysys) finds and loads 
;;;                      "/a/central/registry/path/mysys/mysys.asd"
;;;  SYSTEM-BIN-DIRECTORY: redirects compiler output for a particular system 
;;;            
;;;                                    
;;;                                     
;;;
;;;
#-:asdf (load (merge-pathnames "asdf.lisp" *load-truename*))

;;
;; 
;;
(in-package asdf)

(export 'files-in-system)
(defun files-in-system (system)
  (remove-duplicates 
   (loop for (op . component) in (traverse (make-instance 'operation)
                                           (find-system system))
         when (and (typep component 'component)
                   (stringp (pathname-name (component-pathname component))))
         collect (component-pathname component)) :test #'equalp))

(defun sysdef-central-registry-subsearch (name)
  "Looks in each *central-registry* folder for NAME.asd or system.asd in 
   the subdirectory named NAME."
  (block nil
      (dolist (form *central-registry*)
        (let* ((dir (merge-pathnames (make-pathname :directory `(:relative ,name))
                                     (eval form)))
               (path (when dir
                       (or (probe-file (make-pathname :defaults dir :version :newest
                                                      :name name :type "asd" :case :local))
                           (probe-file (make-pathname :defaults dir :version :newest
                                                      :name "system" :type "asd" :case :local))))))
          (if path (return path))))))

(setf *system-definition-search-functions* (nconc *system-definition-search-functions*
                                                  (list 'sysdef-central-registry-subsearch)))

(export 'platform-name)
(defun platform-name ()
  "Provides a short string which names the current Lisp implementation.  This may
be used to generate directory names for example to store different platform FASL files."
  (flet ((feature-select (elts &optional (default "Unknown"))
           (let ((name (or (first (intersection elts *features*)) default)))
             (if (symbolp name) (string-capitalize (symbol-name name)) name)))
         (make-safe-dir-name (&rest strs)
           (substitute-if-not #\- #'alphanumericp (format nil "~{~A~^-~}" strs))))
    (make-safe-dir-name
     (feature-select '(:allegro :clisp :lispworks :sbcl :cmucl)
                     (lisp-implementation-type))
     #+allegro EXCL::*COMMON-LISP-VERSION-NUMBER* 
     #+clisp (version-major (version))
     #+lispworks system::*major-version-number*
     #-(or :allegro :lispworks :clisp) (lisp-implementation-version)
     (feature-select '(:mswindows :win32 :windows :mac :macos 
                       :macosx :linux :bsd :sgi :irix :unix)
                     (software-type)))))

(export 'component-pathname)
(export 'component-system)

;;;
;;; SYSTEM-BIN-DIRECTORY - function for re-directing compiler output to specific directories 
;;;
;;; USAGE:
;;;         (setf (system-bin-directory :my-system) #P"~/lisp/bin/my-system/")

(defvar *system-bin-directories* ()
  "An assoc list mapping system names to bin directories (which may be relative pathnames)")

(export 'system-bin-directory)
(defun system-bin-directory (system)
  (let* ((system  (find-system system))
         (path    (cdr (find (component-name system)
                             *system-bin-directories* 
                             :key #'car :test #'equalp))))
    (if path (merge-pathnames path (component-pathname system)))))

(defun (setf system-bin-directory) (value system)
  (let* ((system (find-system system))
        (path   (pathname value))
        (sysname (component-name system))
        (existing (find sysname *system-bin-directories* :key #'car :test #'equalp)))
    (cond
     (existing (setf (cdr existing) path))
     (t (push (cons sysname path) *system-bin-directories*)))))


(defmethod output-files :around ((op compile-op) (src source-file))
  (let* ((paths       (call-next-method))
         (system      (component-system src))
         (sys-bin-dir (system-bin-directory system))
         (sys-path    (component-pathname system))
         (src-path    (component-pathname src)))
    (cond 
     (sys-bin-dir (mapcar (lambda (path)
                            (merge-pathnames
                             (make-pathname :type (pathname-type path))
                             (merge-pathnames (enough-namestring src-path sys-path)
                                              sys-bin-dir)))
                          paths))
     (t           paths))))

;;;
;;; SYNTACTIC SUGAR: ASDF:LOAD-SYSTEM
;;;
(export 'load-system)
(defun load-system (system)
  (oos 'load-op (find-system system)))

(pushnew :asdf+ *features*)
