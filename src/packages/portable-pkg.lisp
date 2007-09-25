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

;;; File: portable-pkg
;;; Description: much of the code within lifted from the CLOCC PORTABLE project

;;; $Id: portable-pkg.lisp,v 1.1 2007/09/25 17:54:13 amallavarapu Exp $
;;;
(defpackage portable
  (:use cl)
  #+lispworks (:import-from #:clos #:class-slots)
  (:export ;; mop
   #:class-all-superclasses #:find-direct-superclasses #:is-standard-classp 
   #:finalize-inheritance #:class-finalized-p #:ensure-class #:class-slot-initargs
   #:class-slot-list #:slot-definition-allocation #:slot-definition-one-initarg
   #:slot-definition-name #:class-slots 
   #:class-direct-superclasses #:class-direct-subclasses
   #:copy-instance
           
   ;; prompt functions
   #:prompt-for-yes-or-no

   ;; dspec stuff
   #:variable-information #:declaration-information #:function-information
   #:dspec #:define-dspec-form-parser #:define-dspec-class #:find-dspec-locations
   #:define-dspec-alias #:location #:record-definition  #:location-redefinition-check
   #:*file-extensions* #:destroy-symbol #:destroy-dspec

   ;; misc
   #:user-documents-folder
   #:allowing-redefinitions)
  (:nicknames port))
