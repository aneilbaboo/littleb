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

;;; File: rule
;;; Description: the rule object

;;; $Id: rule.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $
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
