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

;;; File: more-editor-funcs
;;; Description: Does any of this work?

;;; $Id: more-editor-funcs.lisp,v 1.2 2008/09/02 14:58:11 amallavarapu Exp $
;;; $Log: more-editor-funcs.lisp,v $
;;; Revision 1.2  2008/09/02 14:58:11  amallavarapu
;;; Changed to GPL license
;;;
;;; Revision 1.1  2007/09/25 17:54:13  amallavarapu
;;; *** empty log message ***
;;;
;;; Revision 1.5  2007/09/25 16:22:45  am116
;;; *** empty log message ***
;;;
;;; Revision 1.4  2006/11/06 23:40:44  am116
;;; *** empty log message ***
;;;
;;; Revision 1.3  2006/11/02 23:53:26  am116
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
;;; Revision 1.2  2004/11/01 21:14:06  Aneil
;;; *** empty log message ***
;;;
;;;
(in-package editor)


(defun set-buffer-current-package (buffer package)
  (def-ed-var 'current-package :buffer buffer
              package "This is the package we go into for this buffer.")
  (update-buffer-modelines buffer))

(defcommand "BUSER" (p)
     "Put the current window into the b-user package"
     ""
  (declare (ignore p))
  (insert-string (buffers-end (buffer-from-name "Main"))
                 (format t "cl:in-package b-user~%"))
  (editor-eval  (buffer-from-name "Main")
               `(in-package cl-user))
  (set-buffer-current-package (buffer-from-name "Main")
                              (find-package "CL-USER")))
                                     
