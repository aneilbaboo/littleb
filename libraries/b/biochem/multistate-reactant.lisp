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

;;; File: multistate-species.lisp
;;; Description: simple multistate species


;;; $Id: multistate-reactant.lisp,v 1.2 2008/09/02 14:58:10 amallavarapu Exp $

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
