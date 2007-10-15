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


;;; $Id: ode.lisp,v 1.1 2007/10/15 12:48:50 amallavarapu Exp $
;;; Description:  Extends the complex-reaction-type and complex-species-type objects to support ode modeling

(in-package #I@FILE)

(include-declaration :use-packages mallavar-utility)

(include @>/util :use)
(include (@>/biochem
          @>/units/dimensionalization
          @>/math 
          @>/math/ode-var
          @library/biochem/complex) :expose)

(include-documentation :description "Provides extensions to the b/biochem package which enable ODE models to be built."
                       :authors ("Aneil Mallavarapu"))
;;;
;;; For computing the ODE rates:
;;;
(defprop complex-reaction-type._set-rate-fn-args (:documentation "A function or function name which compute the rate of a reaction of this type"))

(defprop complex-reaction-type.k (dictionary :#= [dictionary] :relevance t
                         :documentation "A dictionary of named constants referenced by the rate-fn property"))

(defrule complex-reaction-type-set-rate-fn-on-reaction-types
  (:and [complex-reaction-type-instantiation ?crt ?rt]
   [[complex-reaction-type ?crt] :_set-rate-fn-args ?rfa])
  =>
  ?rt.(apply :set-rate-function ?rfa))

(defield complex-reaction-type.set-rate-function (fn &rest args)
  (setf ._set-rate-fn-args (list* fn args)))


(defprop complex-pattern.t0 ())

(defrule initialize-complex-species-type-t0 
  (:and [complex-pattern-match ?cp ?cst ?iso]
        (?cp [[complex-pattern] :complex-pattern.t0 ?t0]))
  =>
  {?cst.t0 := t0})
