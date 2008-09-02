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

;;; File: littleb.lisp
;;; Description: Loads the little b language.

;;; $Id: littleb.lisp,v 1.9 2008/09/02 14:58:09 amallavarapu Exp $
;;; $Name:  $

#-:asdf+ (load (merge-pathnames "asdf/asdf+.lisp" *load-truename*))


#+:littleb ; if little b is loaded, reset the readtable and print-dispatcher
(setf *readtable* (with-standard-io-syntax *readtable*)
      *print-pprint-dispatch* #+:clisp '(*print-pprint-dispatch*)
                              #-:clisp (with-standard-io-syntax *print-pprint-dispatch*))

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equalp)

(defun delete-littleb-binaries ()
  (ignore-errors (asdf:delete-binaries :b1))
  (ignore-errors (asdf:delete-binaries :graph-tools))
  (ignore-errors (asdf:delete-binaries :lisa)))

#+:clisp (setf custom:*suppress-check-redefinition* t)

#+:clisp (ext:without-package-lock () (asdf:load-system :b1))
#-:clisp (asdf:load-system :b1)


(let ((init-file (b:get-b-path :root "init.lisp")))
  (unless (probe-file init-file)
    (mutils:copy-file (b:get-b-path :root "support/init.lisp") init-file)))

(b:init)

(when (b:library-needs-compile-p 'b)
  (b:compile-library 'b)
  (b:init))

#+:lw-editor
(mutils:whenit (ignore-errors (capi:find-interface 'lispworks-tools:listener))
  (lispworks-tools::listener-execute-command mutils:it "(in-package :b-user)"))

#+:allegro (top-level:do-command :package "B-USER")

#+:lispworks (b::enable-b-key-bindings)

(when (find-package "SWANK")
  (eval (read-from-string "(swank:listener-eval \"(in-package :b-user)\")")))

(unless (eq *package* (find-package :b-user))
  (format t "~%Change to package B-USER: (IN-PACKAGE :B-USER)~%"))
