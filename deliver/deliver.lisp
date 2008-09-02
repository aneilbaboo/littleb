;; -*- Mode: Lisp -*-
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

;; (un)comment features to affect the way this script builds
(push :delete-lw-editor *features*)
(defconstant +compile-features+ '( :compile-library
                                   :copy-all
                                   :copy-libraries
                                   :copy-support
                                   :copy-lisa
                                   :delete-build
                                   ))

(setf *features* (union +compile-features+ *features*))

(in-package "CL-USER")

(load-all-patches)
;; unix complains on delivery


;;; Where we are going to save the application (except on Cocoa)

(defvar *current-dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(defun current-dir (&optional (relpath #P""))
  (merge-pathnames relpath *current-dir*))
(defvar *build-dir* (current-dir "build/"))
(defvar *delivered-image-name* (current-dir (format nil "build/littleb~@[.~A~]"
                                                    #+:win32 "exe" #-:win32 nil)))

#-:asdf (load (current-dir "../asdf/asdf+.lisp"))
(push (probe-file (current-dir #P"../")) asdf:*central-registry*)
(push (probe-file (current-dir #P"../slime/")) asdf:*central-registry*)
(asdf:load-system :swank)
(asdf:load-system :b1)
(compile-file (current-dir "console.lisp") :load t)
(b:init)

#+:compile-library (b:compile-library 'b)

(b:init)
(in-package :cl-user)


;;; require all the packages present in the lispworks environment when little b is loaded
;;; (many of these will be shaken or removed later)
(mapc (lambda (m) (ignore-errors (require m))) '("loop" "B" "describe" "fli-inspector" "inspector-values" "indenting-stream" "LISA" "complex-defsetf" "MAKE" "dde" "delete-selection" "selection-mode" "unshakable-symbols" "delivery-shaker" "file-external-formats" "devenv" "preference" "fli-compilation-support" "GC-INF" "compiler" "defstruct-macro" "xref" "structure-smash" "concatenated-stream" "full" "defsystem" "trace" "advise" "partial-sort" "debug-message" "mp" "xp" "ring" "pcwin32" "defmsgfn-dspecs" "def-repr-dspecs" "type-system-conditions" "fli-conditions" "condition-system" "environment" "clos" "base-clos" "ldatadef" "win32" "fli" "internal-dynamic-modules" "dspec" "template" "coerce" "type-system" "not-really-clos"))

;;;;
;;;; copy libraries and other folders...
;;;;
;;;; This sets up the build folder so that it can function as a valid little b root folder:
;;;             * all libraries are stored in build/libraries
;;;             * root init file (build/init.lisp) sets b:*library-search-paths* to (list (get-b-path :root "libraries/"))
;;;             * a user init file which supports multi-directory installation is provided in build/support
;;;
(defun make-build-folder ()
  (let* ((littleb    (current-dir))
         (littleb*   (merge-pathnames "*.*" littleb))
         (build      *build-dir*)
         (build-libs (merge-pathnames "libraries/" build))
         (root    (b:get-b-path :root))
         (root*   (b:get-b-path :root "*.*")))
  (labels ((copy-lib (libname)
             #+:unix (copy libname :from (b:get-b-path :root "libraries/") :to build-libs)
             #+:win32 (dolist (type `("lisp" ,system:*binary-file-type* "sig"))
                 (copy (make-pathname :name :wild :type type)
                       :from (b:get-b-path :root (format nil "libraries/~A" libname))
                       :to (merge-pathnames libname build-libs)
                       :subdirs t)))
           (copy (src &key (dest src) (from "") (to "") subdirs) ; default from littleb folder
             (let* ((from (merge-pathnames from #+:win32 littleb* #+:unix littleb))
                    (to   (merge-pathnames to build))
                    (src #+:win32 (namestring (merge-pathnames src from))
                         #+:unix (namestring (merge-pathnames src from)))
                    (dest #+:win32 (namestring (make-pathname :name nil :type nil :defaults (merge-pathnames dest to)))
                          #+:unix (namestring to)))
               (ensure-directories-exist dest)
               #+:win32 (format t "~&xcopy ~A ~A ~@[/S~] /Q /Y /D /I~%" src dest subdirs)
               #+:win32 (system:call-system `("xcopy" ,src ,dest
                                                      ,@(if subdirs '("/S")) "/Q" "/Y" "/D" "/I")
                                            :kill-process-on-abort t)
               #+:unix (system:call-system-showing-output `("/bin/cp" "-r" "-p" "--copy-contents" ,src ,dest)) ; complains it can't find program cp.

;               #+:unix (system:call-system-showing-output `("sh" ,copy-script ,src ,dest)) ; this hack doesn't work either; can't find sh
               )))
    
    
    (format t "~&MAKING BUILD FOLDER")

    ;;; so, rely on mutils:
    #+(and :delete-build :win32) (mutils:delete-directory build t)
    #+(and :delete-build :unix) (system:call-system-showing-output `("/bin/rm" "-frd" ,(namestring build)))
    (ensure-directories-exist build)
     
    #+(or :copy-all :copy-libraries) (copy-lib "b/")
    #+(or :copy-all :copy-libraries) (copy-lib "b-user/")
    #+(or :copy-all :copy-libraries) (copy-lib "examples/")
    #+(or :copy-all :copy-libraries) (copy-lib "segment-polarity/")
    #+(or :copy-all :copy-libraries) (copy-lib "scaffold/")
    #+(or :copy-all :copy-support) (copy "support/" :subdirs t)
    #+(or :copy-all :copy-lisa) (copy "lisa/" :subdirs t :from #+:win32 root* #+:unix root)
    (copy "init.lisp" :from (b:get-b-path :root "support/init.lisp"))
    (copy "README.txt" :from (b:get-b-path :root))
    (copy "RELEASE.txt" :from (b:get-b-path :root))
    (copy "CHANGELOG.txt" :from (b:get-b-path :root))
    (copy "LICENSE.txt" :from (b:get-b-path :root))
    (copy "SOURCES.txt" :from (b:get-b-path :root)))))
(make-build-folder)

(in-package :cl-user)
;(compile-file-if-needed (current-pathname "main") :load t
;                        :output-file (pathname-location *delivered-image-name*))

;;; On Cocoa it is a little bit more complicated, because we need to
;;; create an application bundle.  We load the bundle creation code
;;; that is supplied with LispWorks, create the bundle and set
;;; *DELIVERED-IMAGE-NAME* to the value that this returns. We avoid
;;; copying the source files that are associated with LispWorks by
;;; passing :DOCUMENT-TYPES NIL.  When the script is used to create a
;;; universal binary, it is called more than once. To avoid creating
;;; the bundle more than once, we check the result of
;;; SAVE-ARGUMENT-REAL-P before creating the bundle.

;;;; #+cocoa
;;;; (when (save-argument-real-p)
;;;;   (compile-file-if-needed (sys:example-file   "configuration/macos-application-bundle") :load t)
;;;;   (setq *delivered-image-name*
;;;;         (write-macos-application-bundle "~/littleb.app"   
;;;;             :document-types nil)))

(clrhash b::*package-clearable-items*)

(defun existing-packages (plist)
  (delete-if #'null
             (mapcar (lambda (pspec) (if (find-package pspec) pspec)) plist)))


(defun package-begins-with (str)
  (let ((len (length str)))
    (lambda (p) 
      (let ((name (package-name p)))  
        (and (>= (length name) len)
             (equalp (subseq name 0 len) str))))))
(defun packages-beginning-with (str)
  (remove-if-not (package-begins-with str) (list-all-packages)))

;;; Deliver the application
(apply #'deliver 'b-console:run-b-top-level *delivered-image-name* 
         4
         :compact t
         :keep-gc-cursor t
         :multiprocessing t
        ; :display-progress-bar t
         :product-name b-console:*littleb-id-string*
         :format t
         :keep-pretty-printer t
         :keep-package-manipulation t
         :keep-clos t
         :keep-macros t   
         :keep-eval t
         :keep-editor nil
         :keep-documentation t
         :keep-debug-mode t
         :keep-top-level t
         :kill-dspec-table nil
         :keep-conditions :all
         :keep-stub-functions t
         ; doesn't do anything: :gf-collapse-output-file (merge-pathnames "gf-collapse.txt" *load-truename*)
         :remove-setf-function-name nil ; vain attempt to deal with CL:SETF-GET error

      ;   :macro-packages-to-keep '(#:cl #:setf)  ; vain attempt to deal with CL::SETF-GET error
         :packages-to-keep-symbol-names '(#:cl #:setf ) ; vain attempt to deal with CL::SETF-GET error

         :packages-to-keep (existing-packages
                            (append '(#:b #:b-user #:fli-internals #:flii #:pkg
                                          #:cl #:setf                      ; cl & to deal with common-lisp::setf-get
                                          #:b.global #:b #:slot-symbol     ; b 
                                          #:graph-tools
                                          #:lisa-user #:lisa-lisp           ; lisa
                                          )
                                    (packages-beginning-with "SWANK")))

         ;; delete all the b/ library packages:
         :delete-packages (existing-packages (list*
                                              "UFFI" "CAPI" "PKG" "INS" 
                                              "CAPI-LAYOUT" "CAPI-INTERNALS"
                                              "STEPPER" "HQN-WEB" "CAPI-LIBRARY"
                                              "GRAPHICS-PORTS" "COLOR" "CAPI-WIN32-LIB"
                                              (packages-beginning-with "B/")))
         :never-shake-packages '(#:b #:setf #:cl #:mallavar-utility #:lisa #:lisa-user #:cl-user
                                     #:compiler #:slot-symbol #:swank)

                     
         (append
          #+:lispworks4 '(:exit-after-delivery t ; nil - set to nil for debugging purposes
                          :keep-ratio-numbers t
                          :keep-lexer t)
          #+:win32 `(:image-type :exe 
                     :console t)))





