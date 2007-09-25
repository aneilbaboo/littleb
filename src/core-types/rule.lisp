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

;;; File: rule
;;; Description: the rule object

;;; $Id: rule.lisp,v 1.1 2007/09/25 17:54:10 amallavarapu Exp $
;;; $Name:  $

(in-package b)



(defstruct (rule (:conc-name rule-)
                 (:constructor make-rule (symbol  ; symbol which names the rule
                                          patterns ; lhs patterns
                                          lisa-rules
                                          rhs-fn
                                          documentation)))
  symbol patterns lisa-rules rhs-fn documentation)

(defmethod print-object ((r rule) stream)
  (let ((ctx          (print-context-p r)))
  
    (pprint-logical-block (stream nil :prefix (if ctx "" "{") :suffix (if ctx "" "}"))
      (write (rule-symbol r) :stream stream :case :downcase)
      (unless ctx
        (princ " :# " stream)
        (print-unreadable-object (r stream :type t :identity t)
          (prin1 (rule-symbol r) stream))))))
