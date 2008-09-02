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

;;; File: mallavar-utility-pkg.lisp
;;; Description: 

;;; $Id: mallavar-utility-pkg.lisp,v 1.9 2008/09/02 14:58:11 amallavarapu Exp $

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
   #:quit #:gc))
