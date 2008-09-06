;; -*- Mode: Lisp -*-
;;;; This file is part of little b.

;;;; The MIT License

;;;; Copyright (c) 2003-2008 Aneil Mallavarapu

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

#-:asdf (load (current-dir "../asdf/asdf+.lisp"))
(push (probe-file (current-dir #P"../")) asdf:*central-registry*)
(push (probe-file (current-dir #P"../slime/")) asdf:*central-registry*)
(asdf:load-system :swank)
(asdf:load-system :b1)
(compile-file (current-dir "console.lisp") :load t)
(b:init)

(defvar *release-name* (format nil
                               "~A-littleb-console-v~{~A.~A.~A~}"
                               #+:win32 "x86-win32"
                               #+:linux "x86-linux"
                               #+:macos "macos"
                               #-(or :mac :win32 :linux) (error "Please edit deliver.lisp - current platform unknown")
                               (multiple-value-list (b:littleb-version))))
(defvar *build-dir*  (current-dir (format nil "~A/" *release-name*)))
(defvar *delivered-image-name* (make-pathname :name "littleb" :type  #+:win32 "exe" #-:win32 nil
                                              :defaults *build-dir*))
                                                 



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
  (let* ((littleb    (current-dir "../"))
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
                    (to   #+:mac (merge-pathnames src (merge-pathnames to build))
                          #-:mac (merge-pathnames to build))
                    (src #+:win32 (namestring (merge-pathnames src from))
                         #+:unix (namestring (merge-pathnames src from)))
                    (dest #+:win32 (namestring (make-pathname :name nil :type nil :defaults (merge-pathnames dest to)))
                          #+:unix (namestring to)))
               (ensure-directories-exist dest)
               #+:win32 (format t "~&xcopy ~A ~A ~@[/S~] /Q /Y /D /I~%" src dest subdirs)
               #+:win32 (assert (zerop (system:call-system `("xcopy" ,src ,dest
                                                                     ,@(if subdirs '("/S")) "/Q" "/Y" "/D" "/I")
                                                           :kill-process-on-abort t)))
               #+(and :unix (not :mac)) (assert (zerop (system:call-system-showing-output `("/bin/cp" "-r" "-p" "--copy-contents" ,src ,dest)))) ; cygwin complains it can't find program cp.
               #+:mac (assert (zerop (system:call-system-showing-output `("/bin/cp" "-R" "-p" ,src ,dest))))
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
    #+(or :copy-all :copy-lisa) (copy "lisa/" 
                                      :subdirs t
                                      :from #+:win32 root* #+:unix root)
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

;; write a batch file that will deliver the app to littleb.org
;; we can't do this inside this script (which is probably not a great idea anyway)
;; because the :exit-on-delivery keyword to deliver has not been implemented, so
;; nothing can be done after deliver
(with-open-file (file (current-dir #+:win32 "upload.bat" 
                                   #+:unix "upload.sh")
                      :direction :output
                      :if-does-not-exist :create 
                      :if-exists :overwrite )
  #+:win32 (format file
                   "zip -qrv ~A.zip ~:*~A/~%~
                    pscp ~:*~A.zip am116@orchestra.med.harvard.edu:/www/www.littleb.org/docroot/downloads/~%"
                   *release-name*)
  #+:unix (format file 
                  "archiving and compressing ~A~%~
                   tar -cf - ~:*~A/ | gzip -c -9 > ~:*~A.tgz~%~
                   scp ~:*~A.tgz am116@orchestra.med.harvard.edu:/www/www.littleb.org/docroot/downloads/~%"
          *release-name*))
#+:unix (system:call-system-showing-output (list "/bin/chmod" "+x" (format nil "~A.tgz" *release-name*)))
;;;; ;; Deliver the application
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

         ;; :exit-after-delivery nil - not implemented, though it's in the documentation!
         (append
          #+:lispworks4 '(:exit-after-delivery t ; nil - set to nil for debugging purposes
                          :keep-ratio-numbers t
                          :keep-lexer t)
          #+:win32 `(:image-type :exe 
                     :console t)))

