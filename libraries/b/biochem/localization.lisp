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

;;; $Id: localization.lisp,v 1.2 2007/12/16 02:18:50 amallavarapu Exp $


;;; Description:  
;;; LOCALIZATION OBJECT: USED TO REPRESENT {entity @ location}
;;;

(in-package #I@library/biochem)

(defcon localization (:notrace)
  (entity location))

(defield localization.is-valid-for (loc-class)
  (unless (location-class-p loc-class)
    (b-error "Cannot determine whether localization ~S is valid -- invalid location-class (~S)"
             object loc-class))
  (let ((fi   (find .location loc-class._fieldinfos :key ?.symbol)))
    (and (typep fi 'fieldinfo)
         (subtypep (if (mutils:allow-type-p fi.type)
                       (mutils:allow-type-type fi.type) fi.type)
                   'location))))

(defmethod print-object ((o localization) stream)
  (let ((*print-context* t)) (print-math-expression o stream t)))

(defmethod print-math-expression ((o localization) &optional stream outer-op)
  (pprint-math-form `{,o.entity @ ,o.location} stream outer-op))


(defoperator @ ((1- (operator-precedence '+)) :yfx) 
  (rct loc-class)
  [localization rct loc-class])