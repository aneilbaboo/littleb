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

;;; File: math-printer
;;; Description: basic math print-functionality and 
;;;              methods for printing sum-expressions & product-expressions

(in-package b/math)

(defun default-math-printer (o stream &optional (outer-op t))
  "The default math printer used by the little b environment"
  (case o    
    (#\( (princ #\{ stream))
    (#\) (princ #\} stream))
    (t
     (cond
      ((and (not *print-default-operator*)
            (eq o +default-operator+))
       (princ #\space stream))
      ((and *print-context* 
            o._name)                   (print-name o._name stream))

      ((operator o)                  (if (keywordp o) 
                                         (format stream " :~A " o)
                                       (format stream " ~S " o)))
      (t                             (print-math-expression o stream outer-op))))))

(define-generic print-math-expression (o &optional stream outer-p)
  (:documentation "Does the work of printing an infix expression"))

(define-function print-operator (op args &optional (stream *standard-output*) (outer-op t))
  (let ((braces-required (braces-required-p outer-op op)))
    (if braces-required (print-math #\( stream))
    (pprint-logical-block (stream ())
      (ecase (operator-type op)
        ((:xfx :xfy :yfx) 
         (print-math (first args) stream op)
         (print-math op stream)
         (print-math (second args) stream op))
        ((:xf :yf)
         (print-math (first args) stream op)
         (print-math op stream))
        ((:frest)
         (print-math op stream)
         (dolist (a args) (print-math args stream t)))))
    (if braces-required (print-math #\) stream))))
        
       
     
(defparameter *math-print-function* 'default-math-printer
  "This parameter should point to a function of two arguments (object stream) which should print a representation of object to stream")

(defun print-math (o &optional
                     (stream *standard-output*) 
                     (outer-op t))
  "Prints the object o to stream stream using the current *math-print-function*. 
If stream is NIL, a string representation of the math object is returned."
  (cond
   ((null stream)
    (with-output-to-string (stream)
      (funcall *math-print-function* o stream outer-op)))
   (t (funcall *math-print-function* o stream outer-op))))

(defun positive-value (o)
  "Returns 2 value - the absolute value of o, and T if o = the return value"
  (typecase o
    (complex  (values o t))
    (number   (values (abs o) (if (plusp o) t)))
    (quantity (let+ (((abs pos?) (positive-value o.base-magnitude)))
                (values (quantity abs o.dimension) pos?)))
    (sum-expression (if (or (find-if (lambda (se) 
                                       (nth-value 1 (positive-value (sum-element-coef se))))
                                     o.variable)
                            (positive-p o.numeric))
                        (values o t)
                      (values (*op o -1) nil)))
    (t        (values o t))))

(defun positive-p (o)
  (nth-value 1 (positive-value o)))

(defmethod print-math-expression ((o ratio) &optional (stream *standard-output*) left-op)
  (declare (ignore left-op))
  (cond
   ((= (Rationalize (coerce o 'float)) o)
    (let ((str1 (with-output-to-string (s) (prin1 (coerce o 'float) s)))
          (str2 (with-output-to-string (s) (prin1 o s))))
      (if (< (length str1) (length str2)) 
          (princ str1 stream)
        (princ str2 stream))))
   (t   (prin1 o stream))))

(defmethod print-math-expression (o &optional (stream *standard-output*) left-op)
  (let* ((seform (b::self-evalify o))
         (obj-name (b::object-name seform))) 
    (if obj-name ;; check if there's a name 
        (print-math-expression obj-name stream left-op) ;; may invoke the math-name method
      (prin1 seform stream)))) ;; default, just call the underlying print-object method

(defmacro pprint-math-block ((stream braces?) &body body)
  (let ((braces-eval (gensym "BRACES?")))
    `(flet ((math-newline () (pprint-newline-selectively :linear stream)))
       (let ((,braces-eval ,braces?))
         (pprint-logical-block (,stream nil 
                                        :prefix (if ,braces-eval (with-output-to-string (s)
                                                                   (print-math #\( s))
                                                  "")
                                        :suffix (if ,braces-eval (with-output-to-string (s)
                                                                   (print-math #\) s))
                                                  ""))
           ,@body)))))

(defun lower-operator (outer inner)
  "Given 2 operators or precedences, returns the lower of the two in terms of precedence.
The second value indicates whether braces are required if the first argument is considered the
outer operator, and the second argument is considered the inner operator"
 (cond 
   ((eq outer t) (values inner t))
   ((eq outer nil) (values outer nil))
   (t
    (let ((inner-precedence (cond
                           ((numberp inner) inner)
                           ((eq inner t)    (return-from lower-operator (values outer nil)))
                           (t             (operator-precedence inner))))
          (outer-precedence (cond
                           ((numberp outer) outer)
                           (t             (operator-precedence outer)))))
      (if (< outer-precedence inner-precedence) (values outer t)
        (values inner nil))))))

(define-function braces-required-p (outer inner)
  (nth-value 1 (lower-operator outer inner)))

(defparameter *print-magnitude-decimals* 3)

(defun print-magnitude (m stream &optional absp)
  (cond 
   ((numberp m)   (let* ((fixed-mag    (if absp (abs m) m))
                         (fmt-ctrl-str "~,3G");(if *print-magnitude-precision* 
                                       ;    (format nil "~~,~SG" *print-magnitude-precision*)
                                       ;  "~G"))
                         (raw-str      (with-output-to-string (stream)
                                         (format stream fmt-ctrl-str fixed-mag)))
                         (end-pos      (position-if (lambda (o) (not (or (char= o #\space)
                                                                         (char= o #\.))))
                                                    raw-str :from-end t))
                         (trunc-str    (subseq raw-str 0 (min (1+ end-pos) (length raw-str)))))
                    (princ trunc-str stream)))

   (t             (prin1 m stream))))


;;;
;;; INFIX PRETTY-PRINTER
;;;
(defun pprint-as-infix (oform stream &optional (outer-op t))
  "Input: OFORM = operator form := (INFIX-OP ARG1 ARG2...)
   Where each ARG can also be an operator form or any other object.
   E.g., Pretty prints a prefix-style form:  in infix format.
         Each of the symbols at the head of each list must be a defined infix
         operator or NIL.
         E.g., (+ A B (- C D (NIL E)))
               {A + B + C - D E}"          
  (let* ((o       (first oform))
         (args    (rest oform))
         (bracesp (braces-required-p outer-op o))
         (*print-context* t)) 
    (if bracesp (print-math #\( stream))        
    (pprint-logical-block (stream args)
      (ecase (operator-type o)
        ((nil)
         (pprint-exit-if-list-exhausted)
         (loop do (print-math (pprint-pop) stream outer-op)
                  (pprint-exit-if-list-exhausted)
                  (princ #\space stream)))
        ((:xfy :fxy :yfx)
         (print-math (first args) stream o)
         (pprint-newline-selectively :linear stream)
         (print-math o stream)
         (pprint-newline-selectively :linear stream)
         (print-math (second args) stream o))
        ((:xf :yf)
         (print-math (first args) stream o)
         (pprint-newline-selectively :linear stream)
         (print-math o stream))
        (:frest 
         (print-math o stream)
         (pprint-newline-selectively :linear stream)
         (print-math args stream))))
    (if bracesp (print-math #\) stream))))



(defun math-form-to-prefix-form (expr &optional (prefix-operator #'identity))
  (if (math-form-p expr)
      (let ((result (compute-prefix-form (math-form-code expr) prefix-operator)))
        (list* (first result)
               (mapcar (lambda (subexpr)
                         (math-form-to-prefix-form subexpr prefix-operator))
                       (rest result))))
    expr))

(defun pprint-math-form (mform stream &optional (outer-op t))
  "Prints the math form using context sensitive brace printing"
  (pprint-as-infix (math-form-to-prefix-form mform) stream outer-op))