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

;;; File: pkgdecl
;;; Description: package declaration

;;; $Id: pkgdecl.lisp,v 1.23 2008/08/22 15:04:52 amallavarapu Exp $
;;;


(defpackage b
  (:documentation "Contains code for the B language")
  (:use lisa-user cl mallavar-utility portable)
  (:import-from  #:b-system #:*b-root-directory* #:*b-core-signature* #:*b-compile-name*)
  (:export ;; top level forms
   #:define #:defcon #:defprop #:defrule #:predefine #:define-syntax-reader
   #:define-function #:define-generic #:define-method #:defield #:define-macro
   #:kb-transaction #:kb-find-or-create #:add-rule
   #:define-constant #:define-var #:define-parameter
   #:init #:query #:lookup

   #:defstruct-with-fields #:defclass-with-fields
   ;; assumption api
   #:define-assumption-name #:assume #:check-assumption-exists
   ;; include api
   #:*include-verbose* #:*include-force*
   #:include #:include-documentation #:include-declaration #:include-funcall #:include-dynamic
   #:compile-include-path #:reset #:reload-on-reset #:edit #:*reset-target* #:*reset-defaults*
   #:library-create-include-path #:include-file-package-name #:*library-search-paths*
   #:compile-library #:add-library #:libraries #:remove-library #:library-compiled-dir
   #:delete-library-binaries #:library-needs-compile-p
   #:ensure-library #:compile-library  #:library-name #:ignore-paths #:ignore-path-p #:library-compile-directives
   #:find-library #:find-all-libraries #:library-documentation

   ;; include-path information
   #:include-file-documentation #:include-path-library-name
   #:include-path #:include-path-p #:include-path-from-pathname #:include-path-children
   #:include-path-source-file #:include-path-signature #:describe-include-path
   #:include-path-compiled-file #:include-path-compiled-signature
   #:include-path-effective-file #:include-path-parent 
   #:include-path-descendents #:include-path-ancestors #:include-path-components
   #:include-path-package #:include-path-spec 
   #:prompt-for-new-include-file #:prompt-for-library
   ;; data table macros:
   #:with-data-table #:with-substitution-table #:with-permutations #:with-combinations #:macro-multilet 
   ;; expose api
   #:expose-name
   #:expose-symbol #:unexpose-symbol #:symbol-exposed-p #:symbol-exposing-packages
   #:expose-package #:unexpose-package #:package-exposed-p #:package-exposing-packages
   ;; global classes
   #:concept-class #:concept 
   #:property-class #:property
   #:rule #:id
   ;; special symbols
   #:? #:=> #:where #:value #:?field
   #:! #:~ #:@ #:$ #:^ #:& #:& #:* #:_ #:/ #:< #:>
   ;; syntax macros and functions:
   #:object #:fld #:math  #:math-form #:math-form-p #:math-form-code #:math-form-expand
   #:setf-name #:has-name #:object-name
   #:object-form #:object-form-p #:object-form-object #:object-form-args #:object-form-body
   ;; helper macros and functions
   #:add-object-expander #:remove-object-expander #:*object-expanders* #:copy-object-expanders
   #:add-pattern-expander #:remove-pattern-expander #:*pattern-expanders* #:copy-pattern-expanders
   #:object-form-p
   #:object-form-object #:object-form-body #:object-form-args
   #:with-fields #:with-relevance #:global-value 
   #:cclassp #:conceptp #:propertyp #:property-bound-p #:field-bound-p
   #:fld-form #:fld-form-p #:fld-form-object #:fld-form-args #:fld-form-field
   #:missingp #:allow #:xtype #:xtypep
   #:add-operator #:operator  #:print-concept
   #:print-value #:print-name #:pprint-newline-selectively
   #:order #:arbitrary-index #:sort-by-arbitrary-index
   ;; #:concept lambda list keywords
   #:&property #:&optional #:&key #:&rest #:&method
   ;; b object system:          
   #:fieldinfo #:id-fieldinfo #:cclass-fieldinfo #:fieldinfo-symbol #:fieldinfo-lambda-switch
   #:fieldinfo-kind #:fieldinfo-args #:fieldinfo-default #:fieldinfo-documentation
   #:fieldinfo-p #:id-fieldinfo-p #:cclass-fieldinfo-p
   #:local-field-symbol-p #:local-field-symbol
   ;; #:special variables
   #:*relevance* #:*name* #:*math-print-function* #:default-math-printer 
   #:*edit-hook* #:default-edit #:*working-readtable*
   #:get-b-path #:object #:relevance #:property #:nth-object
   ;; printing system
   #:with-print-context #:*print-context*
   #:*debug-printing* 
   ;; monitoring system
   #:*kb-monitor* #:make-quiet-kb-monitor #:make-verbose-kb-monitor #:all-kb-monitor #:make-difference-kb-monitor
   #:memory-monitor
   ;; b error functions and macros
   #:b-error #:b-assert #:b-warn #:*debugger-enabled* #:with-b-error-context
   #:b-error-cause #:b-error-arguments #:b-error-format-string
   ;; system functions
   #:objects #:classes #:unique-object
   #:_name #:enter-b-syntax #:exit-b-syntax
   #:use-b-syntax #:use-cl-syntax #:littleb-version
   ;; required for math-reader
   #:numeric 
   #:+ #:- #:^ #:* #:/ #:= #:< #:> #:<= #:>=                  ; some are redundant, but here for completeness
   #:operator-precedence

   #:quit))

(defpackage "B.GLOBAL")

(defpackage b-user 
  (:use #:b #:cl))
