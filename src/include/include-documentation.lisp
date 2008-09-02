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

;;; File: include-documentation.lisp
;;; Description: Defines the include-documentation top-level include form, and
;;;              provides include-file-documentation, which enables reading of
;;;              documentation from a file.

;;; $Id: include-documentation.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $

(in-package b)

;;;;
;;;; INCLUDE-DOCUMENTATION
;;;; 

(defmacro include-documentation (&body data &key description authors organization version &allow-other-keys)
  "To provide information about the current file."
  (declare (ignorable description data authors version organization))
  (eval-when () ()))

(defun include-documentation-form-p (o)
  (and (consp o) (eq (first o) 'include-documentation)))

(defun describe-include-path (include-path &key
                                           (stream *standard-output*))
  "Prints documentation about the include path to a stream"
  (let* ((ipath   (include-path include-path))
         (src     (probe-file (include-path-source-file ipath))))
    (handler-case 
        (cond
         (src (with-load-ipath (ipath)
                (flet ((expose-form-p (o) (and (include-form-p o) (eql (third o) :expose)))
                       (use-form-p (o) (and (include-form-p o) (eql (third o) :use))))
                  (with-open-file (file-stream src :direction :input)
                    (loop with *package* = (include-path-package ipath t)
                          with pkg-ipath = (include-path *package*)
                          with library =  (find-library ipath) 
                          with eof = '#:eof
                          for form = (read file-stream nil eof)
                          while (include-header-form-p form)
                          if (include-documentation-form-p form)
                          nconc (rest form) into doc-values
                          else if (expose-form-p form)
                          nconc (ensure-list (second form)) into  expose-paths
                          else if (use-form-p form)
                          nconc (ensure-list (second form)) into use-paths
                          else if (include-form-p form)
                          nconc (ensure-list (second form)) into include-paths
                          else if (include-declaration-form-p form)
                          nconc (rest form) into include-declarations
                          finally 
                          (pprint-logical-block (stream nil)
                            (when (not (equalp pkg-ipath ipath))
                              (format stream "Modifies ~A; Use (INCLUDE ~A :USE :MODIFY ~A) ~
                                        when including.~%~%" pkg-ipath ipath pkg-ipath))
                            (print-include-documentation-values doc-values stream)
                            (print-include-form-documentation "INCLUDES & EXPOSES" expose-paths ipath stream)
                            (print-include-form-documentation "INCLUDES & USES" use-paths ipath stream)
                            (print-include-form-documentation "INCLUDES" include-paths ipath stream)
;                          (print-include-form-documentation "INCLUDES SUB-MODULES" 
 ;                                                           same-paths ipath stream)
                            (print-include-documentation-values include-declarations stream)
                            (print-include-documentation-pair "LIBRARY" library stream)))))))
         (t     (format stream "No file associated with this path.")))
      (error (e) (format stream "~A while attempting to read documentation from ~A." e src)))))

(defun print-include-form-documentation (exposure ipaths in-ipath stream)
  (let ((abs-ipaths (remove-duplicates (mapcar (lambda (i) (include-path i in-ipath)) ipaths)
                                       :test #'equalp)))
    (print-include-documentation-pair exposure abs-ipaths stream)))

(defun print-include-documentation-pair (key value stream)
  (labels ((print-value (v stream)
             (typecase v
               (string (pprint-string-breaking-on-whitespace (format nil v) stream))
               (list   (print-value (first v) stream)
                       (dolist (elt (rest v))
                         (princ ", " stream)
                         (pprint-newline-selectively :fill stream)
                         (print-value elt stream)))
               (t      (princ v stream)))))

    (when (and key value)
      (format stream "~A: " key)
      (pprint-logical-block (stream nil)
        (print-value value stream))
      (if *print-pretty* (pprint-newline-selectively :mandatory stream)
        (princ #\newline stream)))))

(defun print-include-documentation-values (doc-values stream)
  (pprint-logical-block (stream nil)
    (map-plist (lambda (k v) (print-include-documentation-pair k v stream))
               doc-values)))

(defun pprint-string-breaking-on-whitespace (string &optional (stream *standard-output*))
  (pprint-logical-block (stream nil)
    (loop for c across string
          do (princ c stream)
          if (whitespace-char-p c)
          do (pprint-newline-selectively :fill stream))))

