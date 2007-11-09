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

;;; File: littleb.lisp
;;; Description: Loads the little b language.

;;; $Id: littleb.lisp,v 1.2 2007/11/09 23:58:04 amallavarapu Exp $
;;; $Name:  $

#-:asdf+ (load (merge-pathnames "asdf/asdf+.lisp" *load-truename*))

(pushnew (make-pathname :name nil :type nil :defaults *load-truename*)
         asdf:*central-registry* :test #'equalp)

#+:clisp (setf custom:*suppress-check-redefinition* t)

#+:clisp (ext:without-package-lock () (asdf:load-system :b1))
#-:clisp (asdf:load-system :b1)

(let ((init-file (b:get-b-path :root "init.lisp")))
  (unless (probe-file init-file)
    (mutils:copy-file (b:get-b-path :root "support/init.lisp") init-file)))

(b:init)

#+:lw-editor
(mutils:whenit (ignore-errors (capi:find-interface 'lispworks-tools:listener))
  (lispworks-tools::listener-execute-command mutils:it "(IN-PACKAGE \"B-USER\")"))

#+:allegro (top-level:do-command :package "B-USER")

#+:lispworks (b::enable-b-key-bindings)

#-(or :allegro :lispworks) (princ "Change to package B-USER: (IN-PACKAGE :B-USER)")