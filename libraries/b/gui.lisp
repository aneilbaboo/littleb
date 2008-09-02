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
          
        
