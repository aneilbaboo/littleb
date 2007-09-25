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

;;; File: reaction-inference.lisp
;;; Description:  when included, this rule infers which species and reactions
;;;               are implied by an initial set of species and reactions.

;;; $Id: reaction-inference.lisp,v 1.1 2007/09/25 17:54:03 amallavarapu Exp $

(in-package #I@FILE)

(include @>/biochem :use)

(defcon reaction-type-requirement-location ()
  (requirement location))

 
(defrule empty-reaction-type-requirement-satisfied
  (:and (?rt-req [reaction-type-requirement 
                  ?rxn-type :lhs nil nil nil])
        (?loc [location])
        (:test (subtypep   (class-of ?loc)  ; the reaction can occur in ?rxn-loc
                           ?rxn-type.location-class)))
  =>
  [reaction ?rxn-type ?loc])

(defrule reaction-type-requirement-satisfied
   (:and (?species   [species ?species-type ?species-loc])    ; and the species in question exists in the right location,        
    [has-sublocation ?rxn-loc ?localization ?species-loc] ; given relationship between a location and sublocation
    (?rt-req    [reaction-type-requirement               
                 ?rxn-type :lhs                     
                 ?species-type ?localization ?stoich])
    (:test (subtypep   (class-of ?rxn-loc)                      ; the reaction can occur in ?rxn-loc
                       ?rxn-type.location-class)))
  =>  ;; one of the reaction-type requirements has been satisfied
  
  ;; record that ?species satisfies the localization requirement ?loc-req
  {?rxn-type.(lhs-species ?rxn-loc ?rt-req) := ?species}
  (when ?rxn-type.(satisfied-at ?rxn-loc)
    [reaction ?rxn-type ?rxn-loc]))