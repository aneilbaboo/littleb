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

