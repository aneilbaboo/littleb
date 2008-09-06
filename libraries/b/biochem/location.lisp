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


;;; File: location
;;; Description: locations where species-types may reside.

;;; $Id: location.lisp,v 1.15 2008/09/06 00:23:08 amallavarapu Exp $


(in-package #I@FOLDER)

(include @/dimensionalization :use)


;;; has-sublocation is a helper concept:
(defcon has-sublocation (:notrace)
  (location field sublocation))

;;;
;;; LOCATION:
;;;
(defcon location (:notrace :abstract) 
  (&property 
   (size :#= (let ((lclass (class-of object)))
               [[reference-var] :dimension (location-class-dimension lclass)
                :value (default-location-size lclass)])
         :relevance t)))

(defun default-location-size (lclass)
  {1 * (location-class-dimension lclass).unit})

(defun location-class-p (o)
  (and (typep o 'standard-class)
       (subtypep o location)))

(deftype location-class ()
  `(satisfies location-class-p))

(defrule compute-has-sublocations-for-name-fields
  (?loc location)
  =>
  (let* ((lclass (class-of ?loc)))
    [has-sublocation ?loc nil ?loc] ; the nil sublocation of a location is itself
    (loop for fi in lclass._fieldinfos
          when (and (id-fieldinfo-p fi) fi.type (subtypep fi.type 'location))
          do [has-sublocation ?loc fi.symbol ?loc.,fi.symbol])))

(defrule compute-has-sublocations-for-properties
  (:and (?prop [|LOCATION.PROPERTY| ?loc ?subloc])
        (:test (location-p ?subloc)))
  =>
  (let* ((fld (class-of ?prop).field-symbol))
    [has-sublocation ?loc fld ?subloc]))
        

(defield location.sublocation (subloc)
  (if subloc object.,subloc
    object))

(defun location-class-is-sublocation (loc-class &rest subloc-fields)
  (let* ((next (first subloc-fields))
         (fi   (find next loc-class._fieldinfos :key ?.symbol))
         (rest (rest subloc-fields)))
    (and (typep fi 'fieldinfo)
         (subtypep (if (mutils:allow-type-p fi.type) 
                       (mutils:allow-type-type fi.type) fi.type)
                   'location)
         (or (null rest)
             (apply #'location-class-is-sublocation (find-class fi.type) rest)))))


(define-function location-class-sublocations (lclass)
  (remove-if-not (lambda (fi)
                   (subtypep (if (mutils:allow-type-p fi.type) 
                                 (mutils:allow-type-type fi.type)
                               fi.type)
                             'location))
                 lclass._fieldinfos))

(define-function containing-location-class (&rest loc-classes)
  "Given one or more location-class objects, returns the one which has all of the others as sublocations"
  (reduce (lambda (x y)
            (let ((xsubloctypes (mapcar ?.type (location-class-sublocations x)))
                  (ysubloctypes (mapcar ?.type (location-class-sublocations y))))
              (cond
               ((eq x y) x)
               ((subtypep (class-name y) `(or ,@xsubloctypes)) x)
               ((subtypep (class-name x) `(or ,@ysubloctypes)) y)
               (t (b-error "Locations of type ~S and ~S cannot be associated with each other"
                           x y)))))
          loc-classes))
              

;;;; ;; simple, atomic locations
(defcon compartment (:notrace location)
  (&optional (id := *name*)
   &property (adjacent-compartments)))

(defcon membrane (:notrace location)
  (&optional (id := *name*)
   &property (outer (allow compartment) :relevance t)
             (inner (allow compartment) :relevance t)))

(defield membrane.c1 () 
  (b-warn "~S.c1: MEMBRANE.C1 is deprecated use MEMBRANE.OUTER instead." object)
  .outer)

(defield (setf membrane.c1) (value) 
  (b-warn "~S.c1: MEMBRANE.C1 is deprecated use MEMBRANE.OUTER instead." object)
  (setf .outer value))

(defield membrane.c2 () 
  (b-warn "~S.c2: MEMBRANE.C2 is deprecated use MEMBRANE.INNER instead." object)
  .inner)

(defield (setf membrane.c2) (value) 
  (b-warn "~S.c2: MEMBRANE.C2 is deprecated use MEMBRANE.INNER instead." object)
  (setf .inner value))

(defun membrane-id-inverse-p (id)
  (and (consp id)
       (eq (first id) :inverse)))

(defun membrane-get-non-inverse-membrane (m)
  (if (membrane-id-inverse-p m.id) m.inverse m))

(defield membrane.inverse ()
  (if (membrane-id-inverse-p .id)
      (second .id)
    (let ((outer .outer)
          (inner .inner))
      {object.inverse :#
       [[membrane (list :inverse object)]
        :inner inner :outer outer]})))
      

(defcon membrane-enclosure ()
  (&optional (inner :#= [compartment])
             (membrane :#= [membrane]))
  (with-relevance t {.membrane.inner := .inner}))

(defield (setf membrane-enclosure.outer) (value)
  {.membrane.outer := value})

(defield membrane-enclosure.outer ()
  .membrane.outer)

(defcon spherical-cell (membrane-enclosure)
  (&optional (inner :#= [compartment])
             (membrane :#= [membrane]))
  (super inner membrane))

(defield spherical-cell.radius ()
  {{.membrane.size.value / {2 pi}} ^ 1/2})

(defield (setf spherical-cell.radius) (value)
  {.membrane.size :@= 2 * pi value ^ 2}
  {.inner.size :@= 4/3 * pi value ^ 3})


;;;
;;; LOCATION DIMENSIONALITY:  compartment size dimensionality 
;;;
(define-generic location-class-dimensionality (lclass)
  (:method (lc) 1)
  (:method ((lc (eql compartment))) *compartment-dimensionality*)
  (:method ((lc (eql membrane))) (1- *compartment-dimensionality*)))

(defun location-dimensionality (o)
  (location-class-dimensionality (class-of o)))

(define-function location-class-dimension (lclass)
  {*distance-dimension* ^ (location-class-dimensionality lclass) })

;;;
;;; DEF-LOCATION-CLASS
;;;
(define-macro def-location-class (name &optional superclass &body sublocations)
  (let ((subloc-fields (mapcar #'first sublocations)))
    (labels ((dot-field (x)      (intern (concatenate 'string "." (symbol-name x))))
             (dot-field-symbol-macro-def (x) `(,(dot-field x) object.,(key x)))
             (process-deflocclass-subloc (x)
               (destructuring-bind (name def &rest prop-def) x
                 (if (and (symbolp def) (ignore-errors (subtypep def 'location)))
                     `(,name :#= [[,def] ,@prop-def])
                   `(,name := [,def ,@prop-def])))))
      `(symbol-macrolet ,(mapcar #'dot-field-symbol-macro-def subloc-fields)           
         (defcon ,name ,superclass
           (&optional (id := *name*)
                      &property ,@(mapcar #'process-deflocclass-subloc sublocations)))))))

