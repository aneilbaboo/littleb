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
(in-package #I@FILE) 

(include @LIBRARY/biochem/dimensionalization :use)
(include @LIBRARY/biochem/multisided-cell :expose)

(check-assumption-exists :biochem-dimensionalization)

(defcon flat-lattice () 
  (&optional (ID := *name*) 
             &property
             (cells := (make-hash-table :test 'equalp))
             (appositions := (make-hash-table :test 'equalp))))
 
(defield flat-lattice.define-apposition 
  (cell-1 cell-1-memb cell-2 cell-2-memb
          &optional
          (size (quantity 1 
                          (location-class-dimension membrane)))
          (ic-size (quantity 1 *compartment-size-dimension*)))
  "Input: is the cells and membranes to be apposed, given as numbers."
  (let* ((m1    (.cell cell-1).membranes.,cell-1-memb)
         (m2    (.cell cell-2).membranes.,cell-2-memb))
    (appose-cell object cell-1 m1 cell-2 m2 size ic-size)))

(defield flat-lattice.define-appositions (apposition-definitions)
  (loop for apposition-definition in apposition-definitions
        do (.apply :define-apposition apposition-definition)))

(defield flat-lattice.define-cells (cell-areas side-lengths) 
  (loop for i from 1 to (length cell-areas)
        for carea = (nth (+ i -1) cell-areas)
        for slength = (nth (+ i -1) side-lengths)
        do (create-cell object                         
                        carea
                        slength i)))

(defield flat-lattice.cell (&rest name)
  (gethash name .cells))

(defield flat-lattice.apposition (cellnum1 cellnum2)
  .appositions.,(list cellnum1 cellnum2))

(defield flat-lattice.do-cells (fn)
  (loop for cell being the hash-value of .cells
        collect (funcall fn cell)))

(defield flat-lattice.do-appositions (fn)
  (loop for app being the hash-value of .appositions
        collect (funcall fn app)))

;;;
;;; HELPER FUNCTIONS:
;;;
(defun appose-cell (lattice cell1num membrane-1 cell2num membrane-2 side-length ic-size)
  "Creates a membrane apposition, named according to which 2 cells are being apposed"
  (let ((ma (_appose-single-cell lattice cell1num membrane-1 cell2num membrane-2)))
    (_appose-single-cell lattice cell2num membrane-2 cell1num membrane-1)
    (with-fields ma
      {.size.value := side-length}
      {membrane-1.size.value := side-length}
      {membrane-1.inverse.size.value := side-length}
      {membrane-2.size.value := side-length}
      {membrane-2.inverse.size.value := side-length}
      {.ic.size.value := ic-size})
    ma))

(defun _appose-single-cell (lattice cell1num membrane-1 cell2num membrane-2)  
  {lattice.(apposition cell1num cell2num) :#
           {lattice.appositions.,(list cell1num
                                       cell2num) := 
                                [membrane-apposition membrane-1 membrane-2]}})

    
(defun create-cell (lattice carea length-list n)
  (let ((cell (make-cell lattice carea n)))
    (loop for i from 1 to (length length-list)
          for adj-membrane  = (+ i -1)  
          for last-membrane = (length length-list)
          do cell.(add-membrane i (nth (+ i -1) length-list))
          (when (and (> i 1) (< i last-membrane)) 
            cell.(define-membrane-interface adj-membrane i)
            cell.(define-membrane-interface i adj-membrane))
  
          (when (eq i last-membrane) 
            cell.(define-membrane-interface i 1)  
            cell.(define-membrane-interface 1 i)  ))   cell))

(defun make-cell (lattice ar num) 
  (let ((cell   {lattice.(cell num) :#
                         {(gethash (list num) lattice.cells) := [multisided-cell]}}))
    {cell.inner.size.value := ar  }
    {cell.size.value := ar}
     
    cell))



















;;;; (defun apposed-cell (cell1 membrane-1 cell2 membrane-2)  
;;;;   (let* ((ma  [membrane-apposition membrane-1 membrane-2 ])) 
;;;;     {(gethash ma cell1.apposed-membranes) := membrane-2} 
;;;;     {(gethash ma cell2.apposed-membranes) := membrane-1} 
;;;;     
;;;;     ma)) 

;;;; (defun make-flat-lattice (verticies cell-list centers cell-areas)
;;;;   (let* ((membranes   (make-hash-table :test 'equalp)) 
;;;;          (lattice     [flat-lattice]))
;;;;     (loop for c in cell-list
;;;;           for i from 1 to (length cell-list)
;;;;           for centroid = (nth (+ i -1) centers)
;;;;           for cell-dim =  (nth (+ i -1) cell-areas) 
;;;;           for cell = (make-cell flat-lattice cell-dim (car centroid) 
;;;;                                 (cdr centroid))          
;;;;           do (loop for p1 in (append (rest c) (list (first c)))
;;;;                    for j from 1 to (length c) 
;;;;                    for adj-membrane  = (+ j -1) 
;;;;                    for p2 in c   
;;;;                    for length = (distance (nth (+ p1 -1) verticies) (nth (+ p2 -1) verticies))  ;what about index 1 or 0 
;;;;                    for mykey = (sort (list p1 p2) #'<)                   
;;;;                    for new-m = (add-membrane cell j length)
;;;;                    for existing-m = (gethash mykey membranes) 
;;;;                   
;;;;                    do                 
;;;;                    (if (eq existing-m nil)
;;;;                        (setf (gethash mykey membranes) (list cell new-m)) 
;;;;                      (appose-cell (first existing-m) (second existing-m) cell new-m))

;;;;                    (if (and (> j 1) (< j (length c))) 
;;;;                        cell.(defmadj j adj-membrane) 
;;;;                      (if (eq j (length c)) 
;;;;                          cell.(defmadj j 1) )))

;;;;          )
;;;;     flat-lattice))


;;;; (defun create-hexagonal-cell (inner-size mlen) 
;;;;   (let ((cell [multisided-cell]))
;;;;     {cell.size.value := 1}
;;;;     {cell.inner.size.value := inner-size } 
;;;;     (loop for i from 1 to 6
;;;;           for adj-membrane  = (+ i -1)  
;;;;           do (add-membrane cell i mlen)
;;;;           (when (and (> i 1) (< i 6)) 
;;;;             cell.(defmadj adj-membrane i)
;;;;                   cell.(defmadj i adj-membrane))
;;;;   
;;;;           (when (eq i 6) 
;;;;             cell.(defmadj i 1)  
;;;;                   cell.(defmadj 1 i)  ))   cell))





;;;; (defun create-cell (lattice carea length-list n) 
;;;;   (let ((cell (make-cell flat-lattice carea n)))
;;;;     (loop for i from 1 to (length length-list)
;;;;           for adj-membrane  = (+ i -1)  
;;;;           for last-membrane = (length length-list)
;;;;           do (add-membrane cell i (nth (+ i -1) length-list))
;;;;           (when (and (> i 1) (< i last-membrane)) 
;;;;             cell.(defmadj adj-membrane i)
;;;;             cell.(defmadj i adj-membrane))
;;;;   
;;;;           (when (eq i last-membrane) 
;;;;             cell.(defmadj i 1)  
;;;;             cell.(defmadj 1 i)  ))   cell))


;;;; (defun create-cell-array (lattice cell-areas side-lengths) 
;;;;   (loop for i from 1 to (length cell-areas)
;;;;         for carea = (nth (+ i -1) cell-areas)
;;;;         for slength = (nth (+ i -1) side-lengths)
;;;;                do (create-cell flat-lattice                         
;;;;                       carea
;;;;                        slength i
;;;;                        
;;;;                       )))  





;;;; (defun make-cell (lattice ar &rest args) 
;;;;   {lattice.(apply :cell args):#
;;;;            {(gethash args flat-lattice.cells) := [multisided-cell]}} 
;;;;   (let ((cell flat-lattice.(apply :cell args))) 
;;;;     {cell.inner.size.value := ar  }
;;;;  {cell.size.value := ar}
;;;;      
;;;;     cell))  

