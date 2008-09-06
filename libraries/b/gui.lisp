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


(in-package b/gui)

(include b/gui/error nil)

(include-documentation :description "Functions for starting the GUI: start, browser, libraries.")

(include-declaration :use-packages (mallavar-utility))

#-(or :lispworks :capi)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Cannot load b/GUI: This package requires LISPWORKS and CAPI."))

(define-function start (&key (stand-alone-mode nil) (kludge-library-compile-sigs nil))
  "Displays the main window."
  ;(when kludge-library-compile-sigs
  ;  (one-time-library-copy-source-signatures-to-compiled (get-b-path :system-library)))
  (include-funcall #I@/main 'show :stand-alone-mode stand-alone-mode))

(define-function file-browser (&key (include-path (include-path *package*))
                                    owner)
  "Displays the file browser"
  (include-funcall #I@/file-browser 'show :include-path include-path :owner owner))

(define-function library-browser (&key library owner)
  "Displays the library browser"
  (include-funcall #I@/library-browser 'show :library library :owner owner))

(define-function load-all ()
  "Ensures that all components of the gui system are loaded."
  (include (#I@/main #I@/file-browser #I@/new #I@/error) nil))

(defun one-time-library-copy-source-signatures-to-compiled (lib)
  (let ((kludge-file (get-b-path :support "kludge-system-signatures")))
    (when (probe-file kludge-file)
      (library-copy-source-signatures-to-compiled (library-name lib))
      (delete-file kludge-file lib))))

(defun library-copy-source-signatures-to-compiled (ipath)
  (let+ (((effective sig)  (include-path-effective-file ipath))
         (cpl-file         (probe-file (include-path-compiled-file ipath))))
    (when (and cpl-file effective
               (not (pathname-equal cpl-file effective)))
      (setf (include-path-compiled-signature path) sig))
    (dolist (child (include-path-children path))
      (library-copy-source-signatures-to-compiled child))))
          
        
