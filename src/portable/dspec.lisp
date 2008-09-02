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

(in-package portable)

(declaim (inline variable-information))
(defun variable-information (symbol &optional environment)
  (declare (ignorable symbol environment))
  #+lispworks 
  (hcl:variable-information symbol environment))

(declaim (inline function-information))
(defun function-information (symbol &optional environment)
  (declare (ignorable symbol environment))
  #+lispworks 
  (hcl:function-information symbol environment))

(declaim (inline declaration-information))
(defun declaration-information (symbol &optional environment)
  (declare (ignorable symbol environment))
  #+lispworks 
  (hcl:declaration-information symbol environment))

;;;
;;; DSPEC Handling - only Lispworks supported so far
;;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro location ()
  (declare (ignorable dspec))
  #+lispworks 
  `(dspec:location)
  #-lispworks ()))

(defmacro at-location ((location &rest extra-args) &body body)
  #+:lispworks
  `(dspec:at-location (,location ,@extra-args)
     ,@body)
  #-:lispworks
  `(let ((*load-truename* ,location)
         (*load-truename* ,location))
     ,@body))

(defmacro dspec (dspec &body body)
  (declare (ignorable dspec))
  #+Lispworks
  `(dspec:def ,dspec ,@body)
  #-Lispworks
  `(progn ,@body))

(defun destroy-dspec (dspec)
  (declare (ignorable dspec))
  #+Lispworks
  (dspec:destroy-dspec dspec)
  #-Lispworks ())

(defun destroy-symbol (symbol &rest args &key undefinep forget-dspec-p uninternp)
  (declare (ignorable symbol args undefinep forget-dspec-p uninternp))
  ()
  #+:lispworks (apply #'dspec:destroy-symbol symbol args)
  #+:clisp (remprop symbol 'system::doc)
  (when (symbol-package symbol)
    (unintern symbol (symbol-package symbol))))
                       
(defun find-dspec-locations (dspec)
  (declare (ignorable dspec))
  #+Lispworks
  (dspec:find-dspec-locations dspec)
  #-Lispworks
  ())

(defun location-redefinition-check (dspec location)
  (declare (ignorable dspec location))
  #+Lispworks
  (let* ((dlocs    (mapcar #'second (portable:find-dspec-locations dspec)))
         (prev     (find-if-not (lambda (dloc) 
                                  (mallavar-utility:pathname-equal dloc location))
                                dlocs)))
    (when prev
      (restart-case (error "Attempt to redefine ~S in ~S (previously in ~S)."
                           dspec location prev)
        (continue () :report "Allow redefinition."
                  (portable:destroy-dspec dspec)))))
  #-Lispworks ())

(defun record-definition (dspec location &optional (check-redefinition-p t))
  (declare (ignorable dspec location check-redefinition-p))
  #+lispworks
  (dspec:record-definition dspec location :check-redefinition-p check-redefinition-p)
  #-lispworks
  nil)

(defmacro define-dspec-class (NAME SUPERSPACE DOCUMENTATION &rest args)
  (declare (ignorable name superspace documentation args))
  #+Lispworks
  `(dspec:define-dspec-class ,name ,superspace ,documentation ,@args)
  #-Lispworks
  ())

(defmacro define-dspec-form-parser (definer-and-options &optional parameters &body body)
  (declare (ignorable definer-and-options parameters body))
  #+Lispworks
  `(dspec:define-form-parser ,definer-and-options ,parameters (declare (ignorable ,definer-and-options)) ,@body)
  #-Lispworks ())

(defmacro define-dspec-alias (name parameters &body body)
  (declare (ignorable name parameters body))
  #+Lispworks
  `(dspec:define-dspec-alias ,name ,parameters ,@body)
  #-Lispworks 
  ())

;;; This code was taken from the MAKE Portable Defsystem written
;;; by Mark Kantrowitz and Marco Antoniotti
(defconstant *file-extensions* 
  (car `(#+(and Symbolics Lispm)              ("lisp" . "bin")
                                              #+(and dec common vax (not ultrix))  ("LSP"  . "FAS")
                                              #+(and dec common vax ultrix)        ("lsp"  . "fas")
                                              #+ACLPC                              ("lsp"  . "fsl")
                                              #+CLISP                              ("lsp"  . "fas")
                                              #+KCL                                ("lsp"  . "o")
                                              #+ECL                                ("lsp"  . "so")
                                              #+IBCL                               ("lsp"  . "o")
                                              #+Xerox                              ("lisp" . "dfasl")
                                              ;; Lucid on Silicon Graphics
                                              #+(and Lucid MIPS)                   ("lisp" . "mbin")
                                              ;; the entry for (and lucid hp300) must precede
                                              ;; that of (and lucid mc68000) for hp9000/300's running lucid,
                                              ;; since *features* on hp9000/300's also include the :mc68000
                                              ;; feature.
                                              #+(and lucid hp300)                  ("lisp" . "6bin")
                                              #+(and Lucid MC68000)                ("lisp" . "lbin")
                                              #+(and Lucid Vax)                    ("lisp" . "vbin")
                                              #+(and Lucid Prime)                  ("lisp" . "pbin")
                                              #+(and Lucid SUNRise)                ("lisp" . "sbin")
                                              #+(and Lucid SPARC)                  ("lisp" . "sbin")
                                              #+(and Lucid :IBM-RT-PC)             ("lisp" . "bbin")
                                              ;; PA is Precision Architecture, HP's 9000/800 RISC cpu
                                              #+(and Lucid PA)		      ("lisp" . "hbin")
                                              #+excl ("cl"   . ,(pathname-type (compile-file-pathname "foo.cl")))
                                              #+(or :cmu :scl)  ("lisp" . ,(or (c:backend-fasl-file-type c:*backend*) "fasl"))
;	 #+(and :CMU (not (or :sgi :sparc)))  ("lisp" . "fasl")
;        #+(and :CMU :sgi)                    ("lisp" . "sgif")
;        #+(and :CMU :sparc)                  ("lisp" . "sparcf")
                                              #+PRIME                              ("lisp" . "pbin")
                                              #+HP                                 ("l"    . "b")
                                              #+TI ("lisp" . #.(string (si::local-binary-file-type)))
                                              #+:gclisp                            ("LSP"  . "F2S")
                                              #+pyramid                            ("clisp" . "o")

                                              ;; Harlequin LispWorks
                                              #+:lispworks 	      ("lisp" . ,COMPILER:*FASL-EXTENSION-STRING*)
;        #+(and :sun4 :lispworks)             ("lisp" . "wfasl")
;        #+(and :mips :lispworks)             ("lisp" . "mfasl")
                                              #+:mcl                               ("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))
                                              #+:coral                             ("lisp" . "fasl")

                                              ;; Otherwise,
                                              ("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))))
  "Filename extensions for Common Lisp. A cons of the form
            (Source-Extension . Binary-Extension). If the system is
            unknown (as in *features* not known), attempts to discover filetype dynamically.")

(defmacro allowing-redefinitions (&body body)
  (or
   #+:lispworks `(let ((hcl:*packages-for-warn-on-redefinition* nil)
		       (LISPWORKS:*HANDLE-WARN-ON-REDEFINITION* :QUIET)
		       (LISPWORKS:*REDEFINITION-ACTION* :QUIET)) ,@body)
   #+:clisp `(ext:without-package-lock () (let ((custom:*suppress-check-redefinition* t)) ,@body))
   #+(or :mcl (and :CCL (not :lispworks))) `(let ((ccl:*warn-if-redefine-kernel* nil)) ,@body)
   #+(and :allegro-version>= (:version>= 4 1)) `(excl:without-package-locks ,@body)
   #+:cmu `(extensions:without-package-locks ,@body)
   `(progn ,@body)))
   
   
   


(defun setup-indent (form-name no-of-args &rest args)
  #+(and :lispworks (not :ignore-lw-editor)) (apply #'editor:setup-indent form-name no-of-args args))
