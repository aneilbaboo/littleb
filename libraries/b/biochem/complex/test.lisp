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

(in-package :b-user)

(include b-user/ode-biochem)

;; tests
(defmonomer mapk a (p :value (member :u :p)))
(defmonomer ksr a b c)
(defmonomer mek a (p :value (member :u :p)))
(defmonomer (egfr membrane) ligand)
(defmonomer egf receptor)
;; examples:

;; complex spliting:
;; {[[ksr 1 _][mapk 1 *]] ->> ksr + mapk}]

;; complex fusion:
;; {[[ksr 1 _][mapk 1 *]] + [[ksr _ 3][mapk 3 :p]]}  '{[[ksr 1 2][mapk 1 :u][ksr 2 3][mapk 3 :p]]}]

;; domain swap
;; {[[ksr 1][mek 1 *]] + [mapk _ *] ->> [[ksr 1][mapk 1]] + [mek * *]}]

;; domain creation
;; {[ksr _ * *] ->> [[ksr 1 * *][mapk 1 :p]]}]

;; domain destruction
;; {[[ksr 1 * *][mapk 1 :p]] ->> [ksr _ * *]}]

;; label change
;; {[[ksr 1 * *][mapk 1 :u]] ->> [[ksr 1 * *][mapk 1 :p]]}]

;; 3-way bond -> 2-way bond 
;; {[[ksr 1 * *][mapk 1 :u][mapk 1 :p]] ->> [[ksr 1 * *][mapk 1 :u]] + [mapk _ :p]}]

(defpackage a (:use cl) (:export reset))
(defun a::reset () 
  (b:init)
  (include b/biochem/complex) 
  ;(eval (read-from-string "[complex-reaction  '{[[ksr 1 * *][mapk 1 *]] ->> [ksr] + mapk}]")) 
  (Format t "{[[ksr 1][mapk 1 *]] ->> [ksr] + [mapk]} [[ksr 1][mapk 1]]~%~%~
             complex fusion:~%~
             {[[ksr 1 _][mapk 1 *]] + [[ksr _ 3][mapk 3 :p]] ->> [[ksr 1 2][mapk 1 :u][ksr 2 3][mapk 3 :p]]} [[ksr 1][mapk 1]] [[ksr _ 1][mapk 1 :p]]~%~%~
             domain swap~%~
             {[[ksr * 1 *][mapk 1 *]] + mek ->> [[ksr * 1 *][mek 1]] + [mapk]} [[ksr 1 2 _][mapk 1 :u][mapk 2 :p]] [[mek _ :p]]~%~%~
             domain creation~%~
             {[ksr * * *] ->> [[ksr 1 * *][mapk 1 :p]]}~%~%~
             domain destruction~%~
             {[[ksr 1 * *][mapk 1]] ->> [ksr _ * *]} [[ksr 1 2 _][mapk 1 :p][mek 2]]~%~%~
             label change~%~
             {[[ksr 1 * *][mapk 1 :u]] ->> [[ksr 1 * *][mapk 1 :p]]} [[ksr 1 2 _][mapk 1 :p][mek 2]]~%~%~
             3-way bond -> 2-way bond ~%~
             {[[ksr 1 * *][mek 1 :u][mapk 1 :p]] ->> [[ksr 1 * *][mek 1 *]] + [mapk _ :p]} [[ksr 1 _ _][mek 1 :u][mapk 1 :p]]~%~%"))

[{[[ksr 1 * *][mek 1 *]] ->> [ksr] + [mek]}
 (.set-rate-function 'mass-action 1)]

[[ksr 1][mek 1]]

[[ksr 1 2][mek 1][mapk 2]]

[[ksr 1 2 3][mek 1][mapk 2][mek 3]]

