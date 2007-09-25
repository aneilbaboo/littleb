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

;;; File: multistate-species.lisp
;;; Description: simple multistate species


;;; $Id: multistate-reactant.lisp,v 1.1 2007/09/25 17:54:03 amallavarapu Exp $
;;; $Log: multistate-reactant.lisp,v $
;;; Revision 1.1  2007/09/25 17:54:03  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.4  2007/09/25 17:40:20  am116
;;; changed to MIT license
;;;
;;; Revision 1.3  2006/11/02 23:53:25  am116
;;; Changed CLEAR/RESET system:
;;;
;;; RESET performs a minimal clear of the system:
;;;     * resets the database
;;;     * clears the *USER-LIBRARY* package (default=B-USER),
;;;     * reloads files which have been registered in the *reload-on-reset* list by RELOAD-ON-RESET
;;;     * loads the file (if any) associated with the package.
;;;
;;; CLEAR resets the database and empties all of the included packages.
;;;
;;; Revision 1.2  2006/10/05 14:59:13  am116
;;;  Little b (including b library) now compiles under Lispworks 4 and 5, Allegro 8, CLisp 2.39.
;;;   * changed DEFMETHOD to DEFIELD for field definitions
;;;   * added string, char readers
;;;   * added hack to deal with CLisp FASL file loader.
;;;   * added a number of MAKE-LOAD-FORM methods for Allegro
;;;   * changed RULE implementation - added the RULE-PARSE object.
;;;
;;; Revision 1.1  2006/09/12 18:02:18  am116
;;; *** empty log message ***
;;;
;;; Revision 1.2  2006/05/30 21:53:40  am116
;;; *** empty log message ***
;;;
;;; Revision 1.1  2006/01/29 19:20:28  am116
;;; INCLUDE system added, and initial system library created.
;;;
;;; Revision 1.7  2005/11/02 17:08:17  am116
;;; * list reader re-written.
;;; * now, local field symbols are ".fieldname", not "@fieldname".
;;;
;;; Revision 1.6  2005/09/22 17:07:25  am116
;;; MAJOR CHANGES:
;;;    * New math subsystem based on product-expression and sum-expression
;;;    * non-dimensionalizing math
;;;    * New approach for separating biology/mechanism/rate-functions
;;;    * Implemented :# / :#= correctly, printing now reflects this
;;;    * removed metaclass as structure support for CLISP (no longer needed)
;;;    * removed reactant-type/reactant, changed these to species-type/species
;;;
;;; Revision 1.5  2005/05/12 20:54:07  am116
;;; *** empty log message ***
;;;

(in-package #I@FILE)
(use-package 'mallavar-utility)
      
  
;;;;
;;;; multistate-species
;;;;
(defcon multistate-species-type ()
  (&key (id := *name*) (location-class location-class := location)
   &property (species-list)))

(defield multistate-species-type.state (&rest state-description)
  (cdr (assoc state-description .species-list #'equalp)))

(defield (setf multistate-species-type.states) (value)
  (with-relevance t
    (setf .species-list 
          (mapcan (lambda (state-descr)
                    (has-name object.(:apply :state state-descr) 
                      (list state-descr [species-type])))
                  value))))

(defield (setf multistate-species-type.sites) (value)
  (let ((state-descrs (compute-choice-paths value)))
    {.states := state-descrs}))
