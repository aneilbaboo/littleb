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


;;; File: multistate-species.lisp
;;; Description: simple multistate species


;;; $Id: multistate-reactant.lisp,v 1.3 2008/09/06 00:23:08 amallavarapu Exp $

(in-package #I@FILE)
(use-package 'mallavar-utility)
      
  
;;;;
;;;; multistate-species
;;;;
(defcon multistate-species-type ()
  (&key (id := *name*) (location-class location-class := location)
   &property (species-list)))

(defield multistate-species-type.state (&rest state-description)
  (cdr (assoc state-description .species-list #'equalp)))

(defield (setf multistate-species-type.states) (value)
  (with-relevance t
    (setf .species-list 
          (mapcan (lambda (state-descr)
                    (has-name object.(:apply :state state-descr) 
                      (list state-descr [species-type])))
                  value))))

(defield (setf multistate-species-type.sites) (value)
  (let ((state-descrs (compute-choice-paths value)))
    {.states := state-descrs}))
