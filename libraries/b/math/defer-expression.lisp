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
        
