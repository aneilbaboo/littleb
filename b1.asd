;; -*- mode:Lisp -*-
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

;;; File: b.asd
;;; Description: little b language system definition file

;;; $Id: b1.asd,v 1.12 2007/12/12 15:20:07 amallavarapu Exp $
;;;
(defpackage #:b-system (:use #:cl #:asdf)
  (:export #:*b-root-directory* #:*b-source-path* #:*b-all-versions-compiled-path*
           #:*b-compile-name* *b-compiled-path*
           #:*b-major-version* #:*b-minor-version* #:*b-revision*
           #:*b-core-signature*
           #:load-b))

(in-package #:b-system)

(defparameter *b-root-directory* (make-pathname :host (pathname-host *load-truename*)
                                           :device (pathname-device *load-truename*)
                                           :directory (pathname-directory *load-truename*)))
(defparameter *b-source-path*    (merge-pathnames "src/" *b-root-directory*))
(defparameter *b-all-versions-compiled-path* (merge-pathnames "bin/" *b-root-directory*))
(defparameter *b-compile-name*   (platform-name))
(defparameter *b-compiled-path*  (merge-pathnames (make-pathname 
                                                   :directory 
                                                   `(:relative ,*b-compile-name*))
                                                  *b-all-versions-compiled-path*))

(defparameter *b-core-signature* 0
  "Determines the revision level of the core by include to determine whether library files should be recompiled.")

(defparameter *b-major-version* 1)
(defparameter *b-minor-version* 5)
(defparameter *b-revision* 5)

(pushnew *b-root-directory* *central-registry* 
         :test (lambda (x y)
                 (ignore-errors (pathname-match-p x y))))

(defsystem :b1
  #-:clisp :source-extension 	#-:clisp "lisp"	
  :depends-on           (:lisa :graph-tools)
  :pathname             *b-source-path*      
  :serial t
  :components
  ((:module #:packages
    :components ((:file "mallavar-utility-pkg")
                 (:file "portable-pkg")
                 (:file "pkgdecl")
                 (:file "pkgconstants"))
    :serial t)
   (:module #:portable
    :components ((:file "dspec")
                 (:file "misc")
                 (:file "clos"))
    :serial t)
   (:module #:utility
    :components ((:file "utility")  
                 (:file "datatable")
                 (:file "form-inspection")
                 (:file "context-info")
                 (:file "predicate")
                 (:file "trie"))
    :serial t)
   (:module #:platform
    :components (#+:lispworks (:file "lispworks")
                 #+:clisp (:file "clisp")))
   (:module #:global
    :components ((:file "constants")
                 (:file "global")
                 (:file "object" :depends-on ("constants"))
                 (:file "error")
                 (:file "fieldinfo")
                 (:file "field" :depends-on ("constants"))
                 (:file "name" :depends-on ("field"))
                 (:file "trace" :depends-on ("field" )) ;"name"))
                 (:file "reader-core" :depends-on ("field")))
    :serial t)
   (:module #:reader
    :components ((:file "token-reader")
                 (:file "list-reader")                               
                 (:file "bq-reader")
                 (:file "field-reader")
                 (:file "object-reader")
                 (:file "math-reader")
                 (:file "string-reader")
                 (:file "char-reader")
                 (:file "quote-reader"))
    :serial t)
   (:module #:core-types
    :components ((:file "concept-class")
                 (:file "concept")
                 (:file "property-class")
                 (:file "property")
                 (:file "rule"))
    :serial t)
    ;:depends-on (#:utility #:reader #:global))                
   (:module #:core
    :components ((:file "rule-patterns")
                 ;(:file "for-pattern")
                 (:file "field-pattern")
                 (:file "lisa-extensions")
                 (:file "order")
                 (:file "printer")
                 ;(:file "error")
                 (:file "query")
                 (:file "kb")
                 (:file "expose"))
    :serial t)
   (:module #:include
    :components ((:file "edit")
                 (:file "include-path")
                 (:file "library")
                 (:file "include-documentation")
                 (:file "include-declaration")
                 (:file "include")
                 (:file "clear")
                 (:file "compile-include-path"))
    :serial t)
   (:module top-level-forms
    :components ((:file "defcon")
                 (:file "define")
                 (:file "defprop")
                 (:file "defrule")
                 (:file "def-with-fields")
                 (:file "assume")
                 (:file "system")
                 (:file "initialize"))
    :serial t))
 ;; can't for the life of me get these PERFORM blocks to run
  :perform (asdf:load-op :after (op c)
             (setf *b-core-signature* 
                   (first (sort (mapcar #'file-write-date (asdf:files-in-system :b1)) #'>)))

;;;;              (flet ((evstr (x) (eval (read-from-string x))))
;;;;                (evstr "(B:INIT)")
;;;;                (evstr "(B:COMPILE-LIBRARY 'B)")
;;;;                (evstr "(B:INIT)"))
                  (pushnew :little-b *features*)
                  (pushnew :b1 *features*)
             (format t "~&; little b loaded successfully~&")))

(setf (system-bin-directory :b1) (format nil "../bin/~A/" (platform-name)))

(defun load-b ()
  (load-system :b1)
  t)
