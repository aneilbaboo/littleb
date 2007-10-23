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

;;; File: location
;;; Description: locations where species-types may reside.

;;; $Id: location.lisp,v 1.3 2007/10/23 17:25:55 amallavarapu Exp $


(in-package #I@FOLDER)

(include @/dimensionalization :use)


;;; has-sublocation is a helper concept:
(defcon has-sublocation (:notrace)
  (location field sublocation))

;;;
;;; LOCATION:
;;;
(defcon location (:abstract) 
  (&property 
   (size :#= [reference-var] :relevance t)))


;;;;   ; how we'd like to be able to do this:
;;;;   ; (query (:and [species ? ?obj]
;;;;   ;        (eq ?obj object))
;;;;   ; (but, too slow currently)
;;;;   ; filthy hack - don't try this at home
;;;;   (loop for obj being the hash-value in b::+objects+
;;;;         when (and (species-p obj)
;;;;                   (eq obj.location object))
;;;;         collect obj))
(defun location-class-p (o)
  (subtypep o location))

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
(defcon compartment (location)
  (&optional (id := *name*)
   &property (adjacent-compartments)
   (size :#= [[reference-var] :dimension *compartment-size-dimension*
              :value {1 *compartment-size-dimension*.unit}] :relevance t)))

(defcon membrane (location)
  (&optional (id := *name*)
   &property (c1 (allow compartment) :relevance t)
             (c2 (allow compartment) :relevance t)
             (size :#= [[reference-var] :dimension *membrane-size-dimension*
                        :value {1 *membrane-size-dimension*.unit}] :relevance t)))
    
(defun membrane-id-inverse-p (id)
  (and (consp id)
       (eq (first id) :inverse)))

(defun membrane-get-non-inverse-membrane (m)
  (if (membrane-id-inverse-p m.id) m.inverse m))

(defield membrane.inverse ()
  (if (membrane-id-inverse-p .id)
      (second .id)
    (let ((c1 .c1)
          (c2 .c2))
      {object.inverse :#
       [[membrane (list :inverse object)]
        :c2 c1 :c1 c2]})))
      

(defcon membrane-enclosure ()
  (&optional (inner :#= [compartment])
             (membrane :#= [membrane]))
  (with-relevance t {.membrane.c2 := .inner}))

(defield (setf membrane-enclosure.outer) (value)
  {.membrane.c1 := value})

(defield membrane-enclosure.outer ()
  .membrane.c1)

(defcon spherical-cell (membrane-enclosure)
  (&optional (inner :#= [compartment])
             (membrane :#= [membrane]))
  (super inner membrane))

(defield spherical-cell.radius ()
  {{.membrane.size.value / {2 pi}} ^ 1/2})

(defield (setf spherical-cell.radius) (value)
  {.membrane.size :@= 2 * pi value ^ 2}
  {.inner.size :@= 4/3 * pi value ^ 3})



(defgeneric location-class-dimension (lclass)
  (:method ((lc (eql compartment))) *compartment-size-dimension*)
  (:method ((lc (eql membrane))) *membrane-size-dimension*))




;;;; ;;;
;;;; ;;; LOCATION-REQUIREMENT - represents a requirement for a species-type in a sublocation of location
;;;; ;;;
;;;; (defcon location-requirement (:notrace)
;;;;   ((type species-type)
;;;;    (localization symbol)))

;;;; (defield location-requirement.is-valid-for (loc-class)
;;;;   (let ((fi   (find .localization loc-class._fieldinfos :key ?.symbol)))
;;;;     (and (typep fi 'fieldinfo)
;;;;          (subtypep (if (mutils:allow-type-p fi.type)
;;;;                        (mutils:allow-type-type fi.type) fi.type)
;;;;                    'location))))
