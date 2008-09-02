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

