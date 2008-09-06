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


;;; File: assume.lisp
;;; Description: API & support for managing assumptions.  
;;;              Assumptions are defined once, and assumed exactly once.
;;;              DEFINE-ASSUMPTION-NAME - defines an assumption named by symbol.
;;;                                       
;;;              Code can check whether an assumption has been made.

;;; $Id $
(in-package b)

(defmacro define-assumption-name (name                       ; a symbol naming the assumption 
                                  &key
                                  (requires            nil)  ; list of other assumption names
                                  (initializer         nil)  ; symbol or lambda list 
                                  (verifier            nil)  ; opt fn for verifying assumption made correctly
                                  (verify-error        nil)  ; form must evaluate to string or nil
                                  (general-error       nil)) ; form must evaluate to string or nil
  (declare (ignorable body))
  `(progn
     (create-assumption-name ',name
                             ',(ensure-list requires)
                             ,verifier
                             ,initializer
                             ,verify-error
                             ,general-error)
     ',name))

(defmacro assume (name &body body)
  (let ((current  (ifit (or *compile-file-truename* *load-truename*)
                      (normalize-pathname it)
                    :listener)))
    `(portable:dspec (assume ,name)
       (kb-touched)
       (progn
         (check-for-duplicate-assumption ',name ,current)
         ,@body
         (post-process-assumption ',name ,current)))))

(defun check-assumption-exists (name)
  (unless (compilingp)
    (check-assumption-name-defined name)
    (let* ((info (assumption-info name))
           (loc  (assumption-info-location info)))
      (and (every (lambda (other) (check-assumption-exists other))
                  (assumption-info-requires info))
           (unless loc
             (check-assumption-exists-error name))
           loc))))

; +------------------------------------+
; +             PRIVATE                +
; +------------------------------------+
(defparameter *assumption-infos* (make-hash-table :test #'equalp))

;;  ASSUMPTION-INFO structure  
(defstruct (assumption-info
            (:constructor make-assumption-info (name 
                                                requires 
                                                verifier initializer
                                                verify-error general-error)))
  name      
  requires   ; a list of other assumption names which must also have been made
  verifier      ; user-fn that returns t if assumption is correct, or nil/error
  initializer       ; user-fn that initializes the assumption
  verify-error   
  exists-error 
  general-error
  location)      ; location where assumption was made

(defun assumption-info (name)
  (check-assumption-name-defined name)
  (gethash name *assumption-infos*))

(defun assumption-info-do-initialize (info)
  (whenit (assumption-info-initializer info)
    (funcall it)))

(defun create-assumption-name (name requires verifier initializer verify-error general-error)
  (when initializer 
    (funcall initializer))
  (let ((ai (make-assumption-info name requires verifier initializer verify-error general-error)))
    (package-mark-item-clearable *package* ai)
    (setf (gethash name *assumption-infos*) ai)))
  
;; errors
(defun duplicate-assumption-error (name existing current)
  (error "Conflicting assumption, (ASSUME ~S), in ~A (previously in ~A).~@[  ~A~]"
          name existing current (assumption-info-general-error (assumption-info name))))

(defun check-assumption-exists-error (name)
  (error "Assumption check failed for ~S.~@[ ~A~]"  
          name (assumption-info-general-error (assumption-info name))))

;; HELPER FUNS
(defun post-process-assumption (name current)
    (verify-assumption name current)
    (setf (assumption-info-location (assumption-info name)) current)
    `(assume ,name ,current))

(defun verify-assumption (name location)
  (let ((info  (assumption-info name)))
    (whenit (assumption-info-verifier info)
      (unless (funcall it)
        (assumption-info-do-initialize info) 
        ;; the default error - the verify fn could throw its own
        (error "Failed to verify assumption ~S in ~A.~@[  ~A~]"
               name location
               (assumption-info-verify-error info))))))

(defun check-for-duplicate-assumption (name current)
  (let* ((info (assumption-info name))
         (existing (assumption-info-location info)))
    (restart-case (when (and existing
                             (not (locations-equal existing current)))
                    (duplicate-assumption-error name existing current))
      (continue () :report "Allow the new assumption, disregarding previous."
        (assumption-info-do-initialize info)))))

(defun check-assumption-name-defined (name)
  (unless (gethash name *assumption-infos*)
    (with-simple-restart (continue "Define the assumption name, ~S." name)
      (error "The assumption name ~S has not been defined - use (DEFINE-ASSUMPTION-NAME ~S)." name name))
    (create-assumption-name name nil nil nil nil nil)))

(defun locations-equal (l1 l2)
  (or (equalp l1 l2)
      (and (pathnamep l1) (pathnamep l2)
           (pathname-equal l1 l2))))

(defmethod clear-item ((ai assumption-info))
  (assumption-info-do-initialize ai)
  (remhash (assumption-info-name ai) *assumption-infos*))

(defun clear-assumptions () 
  (maphash (lambda (k v)
             (declare (ignorable k))
             (assumption-info-do-initialize v))
           *assumption-infos*)
  (clrhash *assumption-infos*)
  t)
      
