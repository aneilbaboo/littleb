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
