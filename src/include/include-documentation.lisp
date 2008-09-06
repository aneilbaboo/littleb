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


;;; File: include-documentation.lisp
;;; Description: Defines the include-documentation top-level include form, and
;;;              provides include-file-documentation, which enables reading of
;;;              documentation from a file.

;;; $Id: include-documentation.lisp,v 1.3 2008/09/06 00:23:09 amallavarapu Exp $

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

