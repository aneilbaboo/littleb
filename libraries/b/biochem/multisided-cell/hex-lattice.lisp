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

;;; File: hex-lattice

;;; Description: 
;;;
;;; $Id: hex-lattice.lisp,v 1.1 2007/09/25 17:54:04 amallavarapu Exp $
;;;
(in-package #I@FILE)

(include-declaration :use-packages (mallavar-utility))

(include @<< :expose)

(defcon hex-lattice ()
  (&optional (id := *name*)
   &property (hex-grid)
             (wrap-vertical)
             (wrap-horizontal)
             (appositions := (make-hash-table :test 'equalp))))

(defun hex-lattice (rows cols 
                         &key
                         (side-length 1)
                         (wrap-vertical t)
                         (wrap-horizontal t))
  (when wrap-vertical
    (assert (evenp rows) (rows)
      "MAKE-HEX-LATTICE requires an even number of rows when WRAP-VERTICAL is set."))
  (let ((lat [[hex-lattice]
              :hex-grid (make-array (list rows cols))
              :wrap-vertical wrap-vertical
              :wrap-horizontal wrap-horizontal]))
    (initialize-hex-lattice lat side-length)))

(defun initialize-hex-lattice (h side-length)
  ;; create the cell array:
  (dotimes (r h.rows)
    (dotimes (c h.cols)
      {h.(cell r c) :# {(aref h.hex-grid r c) := (make-hex-cell side-length)}}))

  (dotimes (r h.rows)
    (dotimes (c h.cols)
      (connect-hex-cell-to-neighbors h r c side-length)))

  h)

(defield hex-lattice.rows ()
  (array-dimension .hex-grid 0))

(defield hex-lattice.cols ()
  (array-dimension .hex-grid 1))

(defun make-hex-cell (side-length &optional inner-size)
  [[polygonal-cell]
   {.size.value := 1 side-length}
   {.inner.size.value := (or inner-size
                             {6 * side-length ^ 2 * 3 ^ 1/2 / 4})} ; 6 * an equilateral triangle
   (.define-closed-membrane `(:top-left ,side-length)
                            `(:top-right ,side-length)
                            `(:right ,side-length)
                            `(:bottom-right ,side-length)
                            `(:bottom-left ,side-length)
                            `(:left ,side-length))])

(defun connect-hex-cell-to-neighbors (lat r c side-length)
  (let ((cell          lat.(cell r c)))
    (flet ((connect (side neighbor-row neighbor-col neighbor-side)
             (let ((membrane cell.membranes.,side))
               (whenit lat.(cell neighbor-row neighbor-col) ; when there's a neighbor,
                 (hex-add-membrane-apposition lat 
                                              cell membrane  ; current cell and membrane
                                              it it.membranes.,neighbor-side
                                              side-length)))))
      (connect :left r (1- c) :right)
      (connect :right r (1+ c) :left)
      (cond
       ((evenp r)
        (connect :top-left (1- r) (1- c) :bottom-right)
        (connect :top-right (1- r) c :bottom-left)
        (connect :bottom-right (1+ r) c :top-left)
        (connect :bottom-left (1+ r) (1- c) :top-right))
       (t ; odd rows
        (connect :top-left (1- r) c :bottom-right)
        (connect :top-right (1- r) (1+ c) :bottom-left)
        (connect :bottom-right (1+ r) (1+ c) :top-left)
        (connect :bottom-left (1+ r) c :top-right))))
    cell))
    

(defield hex-lattice.apposition (c1 c2)
  (gethash (cons c1 c2) .appositions))

(defun hex-add-membrane-apposition (hlat cell1 m1 cell2 m2 side-length)
  (let ((verse {hlat.(apposition cell1 cell2) :# 
                     {(gethash (cons cell1 cell2) hlat.appositions)
                      := (appose-cell cell1 m1 cell2 m2 side-length)}}))
    {hlat.(apposition cell2 cell1) :#
          {(gethash (cons cell2 cell1) hlat.appositions) := verse.inverse}}
    verse))

(defield hex-lattice.cell (r c)
  (let* ((grid .hex-grid)
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (row  (if .wrap-horizontal (mod r rows) r))
         (col  (if .wrap-horizontal (mod c cols) c)))
    (cond
     ((or (< row 0) 
          (>= row rows)
          (< col 0)
          (>= col cols)) nil)
     (t (aref grid row col)))))
