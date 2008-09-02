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

;;; File: portable-pkg
;;; Description: much of the code within lifted from the CLOCC PORTABLE project

;;; $Id: portable-pkg.lisp,v 1.8 2008/09/02 14:58:11 amallavarapu Exp $
;;;
(defpackage portable
  (:use cl)

  (:import-from #:clos
   #-:allegro #:class-slots
   #:funcallable-standard-object
   #:funcallable-standard-class #:set-funcallable-instance-function)
  
  (:export ;; mop
   #:funcallable-standard-object #:funcallable-standard-class #:set-funcallable-instance-function
   #:class-all-superclasses #:find-direct-superclasses #:is-standard-classp 
   #:finalize-inheritance #:class-finalized-p #:ensure-class #:class-slot-initargs
   #:class-slot-list #:slot-definition-allocation #:slot-definition-one-initarg
   #:slot-definition-name #:class-slots 
   #:class-direct-superclasses #:class-direct-subclasses
   #:copy-instance

   ;; funcallable
   funcallable-standard-class set-funcallable-instance-function
           
   ;; prompt functions
   #:prompt-for-yes-or-no

   ;; dspec stuff
   #:variable-information #:declaration-information #:function-information
   #:dspec #:define-dspec-form-parser #:define-dspec-class #:find-dspec-locations
   #:define-dspec-alias #:location #:record-definition  #:location-redefinition-check
   #:*file-extensions* #:destroy-symbol #:destroy-dspec #:at-location #:setup-indent

   ;; misc
   #:user-documents-folder #+:win32 #:get-windows-pathname
   #:make-temp-file        #+:win32 #:*win32-folder-ids*
   #:allowing-redefinitions #:RUN-SHELL-COMMAND)


  (:nicknames port))
