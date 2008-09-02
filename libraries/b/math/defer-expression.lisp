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

(in-package b/math)
(include (@library/util @library/math/math-expression))

(eval-when (:compile-toplevel :load-toplevel :Execute)

(defcon defer-expression (:notrace math-expression)
  (obj function &rest args))

(defield defer-expression.map (fn &rest operator-substitutions)
  (map-vars object fn operator-substitutions))

(defield defer-expression.dimension ()
  (dimension-of object))

(defmacro defer (form)
  (cond
   ((fld-form-p form)   
    `[defer-expression ,(fld-form-object form) 
                       ,(fld-form-field form)
                       ,@(fld-form-args form)])
   ((consp form)        (unless (symbol-function (first form))
                          (b-error "Invalid: {defer (~S ~{ ~S})}" form))
    `[defer-expression nil ',(first form) ,@(rest form)])
   (t (b-error "invalid {defer ~S}" form))))

(defun defer-map-vars (o fn operator-substitutions)
  (if (math-expression-p o)
      (map-vars o fn operator-substitutions)
    o))

(defmethod map-vars ((d defer-expression) fn &optional operator-substitutions)
  (let ((d-o           (map-vars (|DEFER-EXPRESSION.OBJ| d) fn operator-substitutions))
        (d-fn          (|DEFER-EXPRESSION.FUNCTION| d))
        (d-args        (mapcar (lambda (arg) 
                                 (map-vars arg fn operator-substitutions))
                               (|DEFER-EXPRESSION.ARGS| d))))
    (cond
     (d-o (fld d-o :apply d-fn d-args))
              
     (t   (apply d-fn d-args)))))

(defmethod dimension-of ((o defer-expression))
  (dimension-of (force-dimensional-math (|MATH-CONCEPT.LET| o))))
                          

(defmethod print-object ((o defer-expression) stream)
  (pprint-math-block (stream t)
    (if o.obj (format stream "defer ~S.(~S~{ ~S~})" o.obj o.function o.args)
      (format stream "defer (~S~{ ~S~})" o.function o.args))))
    
(add-operator 'defer +op-precedence-expt+ :fx 'defer)
)



(defun replace-form-when-evaled (bindings code-form)
  (cond
   ((consp code-form)
    (let ((head (first code-form))
          (args (rest code-form)))
      (cond
       ((special-operator-p head)
        (replace-special-operator-evaled-forms bindings head args))
     
       ((macro-function head)
        (replace-form-when-evaled bindings (macroexpand code-form)))
     
       (t
        (cons head (replace-seq-with-bindings bindings args))))))
    (t (replace-with-bindings bindings code-form))))
  
(defun replace-seq-with-bindings (bindings seq)
  (mapcar (lambda (arg) (replace-with-bindings bindings arg))
          seq))

(defun replace-with-bindings (bindings o)
  (ifit (assoc o bindings :test #'equalp)
      (second it)
    (if (consp o)
        (replace-form-when-evaled bindings o)
      o)))

(defun replace-special-operator-evaled-forms (bindings head args)
  (flet ((replaced-args (head args bindings)
           (ecase head
             ;; no args
             ((quote function go) args)
             
             ;; (&rest args) - all args evaled
             ((throw if multiple-value-call progv unwind-protect load-time-value
                     multiple-value-prog1)                                
              (replace-seq-with-bindings bindings args))
          
             ;; (&body declare-body)
             ((progn)
              (replace-seq-with-bindings bindings (remove-declare-forms args)))
          
             ;; (not-evaled &body body)
             ((block catch eval-when the return-from)
              (cons (first args) (replace-seq-with-bindings bindings (rest args))))
          
             ;; (not-evaled &body declare-body) - first not evaled, rest evaled
             ((flet labels macrolet symbol-macrolet)
              (cons (first args) (replace-seq-with-bindings bindings (remove-declare-forms (rest args)))))
    
             ;; (bindings &body body)
             ((let let*)                                             
              (cons (mapcar (lambda (lbind) 
                              (list (first lbind) (replace-with-bindings bindings (second lbind))))
                            (first args))
                    (replace-seq-with-bindings bindings (remove-declare-forms (rest args)))))

             ;; (place evaled-form)*
             ((setq)                                      
              (replace-setq-args-with-bindings bindings args))

             ;; tagbody 
             ((tagbody)                      
              (mapcar (lambda (o) (if (symbolp o) o
                                    (replace-with-bindings bindings o)))
                      args)))))
    (cons head (replaced-args head args bindings))))

(defun remove-declare-forms (body)
  (let ((pos-non-decl (or (position-if-not (lambda (o) (and (consp o) (eq (first o) 'declare))) body)
                          0)))
         
    (subseq body pos-non-decl)))

(defun replace-setq-args-with-bindings (bindings seq)
  (loop for elt in seq
        for evaled = nil then (not evaled)        
        if evaled 
        collect (replace-with-bindings bindings elt)
        else
        collect elt))
        
