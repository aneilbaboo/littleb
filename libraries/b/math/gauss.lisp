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

;;; File: gauss
;;; Description: defines the gaussian object
;;; test with: http://jan.ucc.nau.edu/~wittke/Microprobe/Statistics-Error.html

;;; $Id $
;;; $Log $
(in-package b/math)

(defcon gauss (numeric-concept)
  (mean stdev))

(defmethod dimension-of ((g gauss)) null-dimension)
(defmethod magnitude-of ((g gauss) &optional u)
  (declare (ignore u))
  g.mean)

(defield gauss.random-value ()
  (+ .mean (* .stdev (box-muller))))
  
(defun box-muller ()
  "Returns 2 random numbers with a gaussian probability distribution about 1, with a stdev of 1"
  (flet ((rnd () (/ (random most-positive-fixnum) most-positive-fixnum)))
    (let ((x1 0)
          (x2 0)
          (w 1.0))
      (loop while (>= w 1.0)
            do (setf x1 (- (* 2.0 (rnd)) 1.0)
                     x2 (- (* 2.0 (rnd)) 1.0)
                     w  (+ (* x1 x1) (* x2 x2))))
      (let ((w (sqrt (/ (* -2.0 (log w)) w))))
        (values (* x1 w) (* x2 w))))))
      
            

(defmethod negativep ((g gauss))
  (negativep g.mean))

(defmethod convert-to-unit ((g gauss) u &optional inverse power)
  (gauss (convert-to-unit g.mean u inverse power) (convert-to-unit g.stdev u inverse power)))

(defmethod +op ((g1 gauss) (g2 gauss))
  (flet ((add-stdev (s1 s2)
           (sqrt (+ (* s1 s1) (* s2 s2)))))
    (gauss (+ g1.mean g2.mean) (add-stdev g1.stdev g2.stdev))))

(defmethod +op ((g gauss) (n number))
  (gauss (+ g.mean n) g.stdev))

(defmethod *op ((g gauss) (n number))
  (gauss (* g.mean n) (* n g.stdev)))

(defmethod *op ((g1 gauss) (g2 gauss))
  (let* ((mean-prod (* g1.mean g2.mean))
         (stdev     (* mean-prod (relative-stdev-muldiv g1 g2))))
    (gauss mean-prod stdev)))

(defmethod *op ((g1 gauss) (u unit))
  (quantity (gauss (convert-to-unit g1.mean u t)
                   (convert-to-unit g1.stdev u t))
            u.dimension))

(defmethod *op ((q quantity) (g gauss))
  (quantity (*op q.base-magnitude g) q.dimension))

(defmethod ^op ((g gauss) (n number))
  (let* ((gexpm  (careful-expt g.mean n))
         (relstd (/ g.stdev g.mean))
         (gexps  (* gexpm (* (sqrt (abs n)) relstd))))
    (gauss gexpm gexps)))

(defun relative-stdev-muldiv (g1 g2)
  "Given 2 gaussians, calculates the stdevs for multiplication or division"
  (let* ((g1m      g1.mean)
         (g2m      g2.mean)
         (g1e      (/ g1.stdev g1m))
         (g2e      (/ g2.stdev g2m)))
    (sqrt (+ (* g1e g1e) (* g2e g2e)))))

(defun gauss (mean stdev)
  (assert (and (realp mean) (realp stdev)) ()
      "Gaussians must be constructed from two real values, but ~S +- ~S was provided." 
      mean stdev)
  (if (zerop stdev) mean
    #[gauss (rationalize mean) (rationalize stdev)]))

(add-operator '+- (1- +op-precedence-muldiv+) :xfy #'gauss)
  
(defmethod print-math-expression ((g gauss) &optional (stream *standard-output*) (lhs-op t))
  (let ((braces? (braces-required-p lhs-op '+-)))
    (with-print-context g
      (pprint-math-block (stream braces?)
        (print-magnitude g.mean stream)
        (princ " +- " stream)
        (print-magnitude g.stdev stream)))))
  
(defmethod print-object ((g gauss) stream)
  (if *debug-printing* (call-next-method)
    (print-math-expression g stream)))
    
