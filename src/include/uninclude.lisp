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

(in-package b)

;;;;
;;;; UNINCLUDE
;;;;
(defun uninclude (&optional (include-path (include-path *package*)) (method :unuse))
  "Attempts to remove include-path.  This means that the file is removed and the package is deleted.
Treatment of packages which depend on this include-path is handled by the the METHOD parameter, which may be one of:
   NIL        - uninclude ONLY if there are no dependent packages
   :UNUSE     - unuse dependent packages
   :RECURSIVE - unuse & uninclude dependent packages"
  (clear-include-path (include-path include-path) method))

(defun clear-include-path (ipath method)
  (flet ((make-package-unused (pkg using-pkgs)
           (dolist (upkg using-pkgs)
             (setf (include-path-current-signature (include-path upkg)) nil)
             (unuse-package pkg upkg))))
    (let* ((pkg     (include-path-package ipath nil))
           (used-by (if pkg (package-used-by-list pkg)))
           (delete? (if (and pkg (include-path-exists-p ipath))
                        (ecase method
                          ((NIL)      (unless used-by t))
                          (:unuse     (make-package-unused pkg used-by) t)
                          (:recursive (make-package-unused pkg used-by)
                           (dolist (upkg used-by)
                             (uninclude upkg :recursive))
                           t)))))
      (setf (include-path-current-signature ipath) nil)
      (when delete?  
        (cond
         ((eq *package* pkg)  (dolist (up (package-use-list pkg))
                                (unuse-package up))
                              (do-all-symbols (s)
                                (unintern s))
                              (use-package '(cl b)))
         (t                   (delete-package pkg)))
        t))))

