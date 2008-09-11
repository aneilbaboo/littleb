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


;;; File: mallavar-utility-pkg.lisp
;;; Description: 

;;; $Id: mallavar-utility-pkg.lisp,v 1.11 2008/09/11 21:50:08 amallavarapu Exp $

;;;

(defpackage mallavar-utility
  (:use cl)
  (:nicknames mutils)
  (:export delete-package-tree #:split #:splice #:insert 
   #:insert-after-if #:ninsert-after-if #:insert-after #:ninsert-after   
   #:find-and-replace #:find-and-replace-all #:assoc-to-list #:assoc-val
   #:delete-nth #:remove-nth #:nexclusion #:exclusion #:multiset-same-p
   #:copy-hash-table  
   ensure-cons #:ensure-list #:find-duplicates 
   #:it ; must export IT because some lisps (e.g., lispworks mac 5) do macroexpansion at function run-time (not define-time)
   #:ifit #:whenit #:condit
   let+ #:contains-duplicates #:all-same #:string-invertcase 
   delim-mkstr #:delim-sym #:mkstr #:find-extra #:remove-position
   make-type-sort-predicate #:quoted-p #:pushend #:pushnew-first
   sym #:reread #:key #:maptree #:mapatoms #:map-plist #:mapp #:assoc-list-from-plist #:keyword-list-p
   #:tok #:unprefix #:external #:symbol-external-p #:maphash-to-list #:compose
   #:positions #:positions-if
   mvbind #:mvlist #:mvsetq #:mvcall #:mvprog1 #:parse-key-list
   xor #:bool #:print-class-unreadably #:careful-expt #:with 
   delete-directory #:pathname-descendent-p #:pathname-directory-p #:normalize-pathname #:pathname-equal #:pathname-parent #:pathname-ancestors
   let*protect #:compute-choice-paths #:copy-file #:pair-plist
   destructure-method-body #:make-child-pathname
   def-multitype-method #:def-commutative-operator-method #:collect-symbols
   #:timed-dolist #:decode-absolute-time #:macroexpand-n
   #:match #:test
   #:with-compile-path #:with-load-path
   ;; datatable macros:
   ;#:with-data-table #:with-substitution-table #:with-combinations #:with-permutations #:macro-multilet
   ;; context info:
   *context-info* #:with-context-info #:error-with-context #:format-context-info
   ;; form inspection:
   compute-form-type #:compute-form-class #:compute-form-value #:constant-value
   #:compile-time-computable-form-p
   the-expr-p #:the-expression #:the-expr-type #:the-expr-form #:the-expr-reduce-to-form
   allow #:allow-type-p #:allow-type-type #:allow-expr-p #:allow-expr-type #:allow-expr-form
   #:with-binding-environment #:def-binding-environment
   ;; environment code:
   #:quit #:gc
   ;; misc
   #:eval-string))
