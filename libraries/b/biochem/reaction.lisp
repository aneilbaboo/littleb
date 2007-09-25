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

;;; File: reaction.lisp
;;; Description:

;;; $Id: reaction.lisp,v 1.1 2007/09/25 17:54:03 amallavarapu Exp $

(in-package #I@FOLDER)

(include @/reaction-type)

(defcon reaction ()
  ((type      reaction-type)
   (location  location))
  =>
  (setf (gethash .location .type.reactions) object)
  (has-name .type.(in .location) object)
  (satisfy-requirements .type :lhs .location)
  (satisfy-requirements .type :rhs .location))


(defield reaction.reactants ()
  (gethash .location .type.reactants))

(defield reaction.products ()
  (gethash .location .type.products))

(defun satisfy-requirements (rxn-type side loc)
 ; (assert (member side '(:rhs-requirements :lhs-requirements)))
  ;; ensure RHS localization-requirements are satified:
  (mutils:let+ (((reqs spc) (ecase side
                                (:lhs (values :lhs-requirements :lhs-species))
                                (:rhs (values :rhs-requirements :rhs-species)))))
    
    (dolist (req rxn-type.,reqs)
      {rxn-type.(,spc loc req) 
                := [species req.species-type 
                            loc.(sublocation req.localization)]})))
  
