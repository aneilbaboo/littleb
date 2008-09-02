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


;;;
;;; edit - 
;;;
(defvar *edit-hook* 'default-edit
  "A function of one argument (a pathname) which may be provided by an IDE environment to catch edit requests.")

(defun prompt-if-probe-file-fails (file)
  (or (probe-file file)
      (when (yes-or-no-p "Cannot find ~A - create file?" file)
        (with-open-file (stream file :direction :output :if-does-not-exist :create))
        (probe-file file))))

(defun edit (&optional x)
  (let* ((ipath        (ignore-errors
                         (typecase x
                           (null   (include-path *package*))
                           (keyword (include-path x))
                           (symbol (let ((*package* (symbol-package x)))
                                     (include-path x)))
                           (t      (include-path x)))))
         (ipath-file   (include-path-source-file ipath))
         (dspec-loc    (ignore-errors (second (first (portable:find-dspec-locations x)))))
         (thing        (or (if dspec-loc x)
                           (if ipath-file (prompt-if-probe-file-fails ipath-file))
                           (ignore-errors (probe-file (pathname x))))))
    (when ipath-file
      (ignore-errors
        (include-path-package ipath t)))
    (when (and thing *edit-hook*)
      (funcall *edit-hook* thing))
    thing))

(defun default-edit (x)
  "Attempts to open the include path or pathname with the editor"
  (ed x))


;;;;
;;;; edit-and-retry
;;;;
(defvar *edit-and-retry-streams* :toplevel
  "A list of streams which should be closed before editing a file")

(defmacro with-edit-and-retry-restart ((file &key (display-name file) (restart-name 'edit)) &body body)
  "Where file is the pathname of the file to edit, display-name is an object which is printed (as by PRINC), and test
is a lambda expression or symbol denoting a function of one argument which returns a generalized boolean indicating
whether the restart should be displayed."
  (let ((reload (gensym "RELOAD")))
      ;  (condition (gensym "CONDITION")))
    `(let ((*edit-and-retry-streams* nil))
       (tagbody 
        ,reload
        (restart-case (progn ,@body)
          (,restart-name (,reload) 
;;;;                 :test (lambda (,condition)
;;;;                         (declare (ignorable ,condition))
;;;;                         (and ,@(when test `((test ,condition))) ,file (probe-file ,file)))
                :report (lambda (stream)
                          (format stream "Edit ~A" ,display-name))
                :interactive (lambda ()
                               (edit-and-retry-prompt ,file))
                (if ,reload (go ,reload)
                  (abort))))))))

(defmacro with-open-file-for-edit-and-retry ((stream file &rest options) &body body)
  "Used inside a with-edit-file-and-continue block to safely open a stream which may need to
be edited."
  `(with-open-file (,stream ,file ,@options)
     (unless (eql *edit-and-retry-streams* :toplevel)
       (push ,stream *edit-and-retry-streams*))
     ,@body))
     
(defun edit-and-retry-prompt (file)
  (dolist (stream *edit-and-retry-streams*)
    (ignore-errors (close stream)))
  (ignore-errors (edit file))
  
  (when *debug-io*
    (or (ignore-errors
          (loop for c = (progn (format *debug-io* "~&Reload (R) or Abort (A): ")
                          (read-char *debug-io*))
                if (char-equal c #\r)
                do (return (list t))
                if (char-equal c #\a)
                do (return (list nil))))
        (list nil))))


;;;; (defmacro with-edit-ipath-and-continue-restart (ipath &body body)
;;;;   `(with-edit-and-retry-restart (,ipath (include-path-source-file ,ipath
;;;;                                                                      (ensure-library ,ipath))
;;;;                                            (lambda (c) (declare (ignorable c))
;;;;                                              (ensure-library ,ipath)))
;;;;      ,@body))
