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
;;;
;;; examples:
;;;    a species: [[ksr !1 _ _] [mapk !1]]
;;;    a reaction: {[ksr _] + [mapk _] -> [[ksr !1][mapk !1]]}
;;;            ==  {[ksr _1 * *] + [mapk _1 * *] -> [[ksr !1 * *][mapk !1 * *]]

;;;
;;;    [[d1-s-!1 *][d2-s-!1 *]] [d3...] -> [[d1 _]] + [d2-s!1-s!1-d3] 
;;;
;;;                                 -> might actually be [[d1 _ !2] 
;;;

;;;
;;; SITE - describes a site - 
;;; DOMAIN - a description of a fragment of a graph
;;; COMPLEX - a species-type which 
;;;
(in-package b-user)

(include b/biochem :expose)

(include (@file/complex-species-type @file/complex-reaction))

;; tests
(defdomain mapk a (p :value (member :u :p)))
(defdomain ksr a b c)
(defdomain mek a (p :value (member :u :p)))

;; examples:

;; complex spliting:
;; [complex-reaction  '{[[ksr 1 _][mapk 1 *]]} '{ksr + mapk}]

;; complex fusion:
;; [complex-reaction  '{[[ksr 1 _][mapk 1 *]] + [[ksr _ 3][mapk 3 :p]]}  '{[[ksr 1 2][mapk 1 :u][ksr 2 3][mapk 3 :p]]}]

;; domain swap
;; [complex-reaction  '{[[ksr 1][mek 1 *]] + [mapk _ *]} '{[[ksr 1][mapk 1]] + [mek * *]}]

;; domain creation
;; [complex-reaction  '{[ksr _ * *]} '{[[ksr 1 * *][mapk 1 :p]]}]

;; domain destruction
;; [complex-reaction  '{[[ksr 1 * *][mapk 1 :p]]} '{[ksr _ * *]}]

;; label change
;; [complex-reaction  '{[[ksr 1 * *][mapk 1 :u]]} '{[[ksr 1 * *][mapk 1 :p]]}]

;; 3-way bond -> 2-way bond 
;; [complex-reaction  '{[[ksr 1 * *][mapk 1 :u][mapk 1 :p]]} '{[[ksr 1 * *][mapk 1 :u]] + [mapk _ :p]}]

(defpackage a (:use cl) (:export reset))
(defun a::reset () 
  (b:init)
  (include b/biochem/complex) 
  ;(eval (read-from-string "[complex-reaction  '{[[ksr 1 * *][mapk 1 *]]} '{ksr + mapk}]")) 
  (Format t "(compute-reaction-output  '{[[ksr 1][mapk 1 *]]} '{ksr + mapk} '(((ksr 1)(mapk 1))))~%~%~
             complex fusion:~%~
             (compute-reaction-output  '{[[ksr 1 _][mapk 1 *]] + [[ksr _ 3][mapk 3 :p]]}  '{[[ksr 1 2][mapk 1 :u][ksr 2 3][mapk 3 :p]]} '(((ksr 1)(mapk 1))((ksr _ 1)(mapk 1 :p))))~%~%~
             domain swap~%~
             (compute-reaction-output  '{[[ksr * 1 *][mapk 1 *]] + mek} '{[[ksr * 1 *][mek 1]] + mapk} '(((ksr 1 2 _)(mapk 1 :u)(mapk 2 :p)) ((mek _ :p))))~%~%~
             domain creation~%~
             (compute-reaction-output  '{[ksr * * *]} '{[[ksr 1 * *][mapk 1 :p]]}  )~%~%~
             domain destruction~%~
             (compute-reaction-output  '{[[ksr 1 * *][mapk 1]]} '{[ksr _ * *]} '(((ksr 1 2 _)(mapk 1 :p)(mek 2))))~%~%~
             label change~%~
             (compute-reaction-output  '{[[ksr 1 * *][mapk 1 :u]]} '{[[ksr 1 * *][mapk 1 :p]]} '(((ksr 1 2 _)(mapk 1 :p)(mek 2))))~%~%~
             3-way bond -> 2-way bond ~%~
             (compute-reaction-output  '{[[ksr 1 * *][mek 1 :u][mapk 1 :p]]} '{[[ksr 1 * *][mek 1 *]] + [mapk _ :p]} '(((ksr 1 * *)(mek 1 :u)(mapk 1 :p))))~%~%"))
