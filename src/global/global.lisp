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

;;; File: 
;;; Description: 
;;; Defines globally-accessible symbols:
;;;
;;; Globals are symbols which may have global or lexical values.  Unlike
;;; CL's special variables (e.g., ala defparameter defvar), binding is
;;; not dynamic.  

;;;
;;; Top-level-forms:
;;;       define-global-const (s form)       => symbol expands to a hidden defconstant
;;;       define-global-block-info (s form)   => symbol expands to a function 
;;;                                        which accesses a hidden var.  
;;;                                        global forms may be reset in the order of definition.
;;;       define-global-var (s form)         => symbol expands to a hidden defvar
;;; ALT:  define-global-var (s type form)       (in this alternate form, type info is provided)
;;;       predefine ((symbols...) body) => treats occurrences of symbols in body
;;;                                        as globals (even before it has been defined)
;;; Helpers:
;;;       globalp (s)                   => tests whether s is a global-symbol
;;;       global-value (s)              => obtains the global value of s in any lexical context.

;;; Justification:
;;;
;;; Dynamic binding can produce very difficult to find bugs, since a variable
;;; may be inadvertently bound, and thus hold an incorrect value later on.
;;; The CL approach around this problem is to adopt a naming convention which
;;; places asterisks or plus signs around the symbol name: e.g, *MYVAR*, +MYCONST+
;;;
;;; In b, this is undesirable since these symbols will be VERY frequently used.
;;; The addition of two characters for every symbol increases line length, making
;;; code harder to read.
;;;
;;; Here's a more detailed description of problems w/ constants & special vars:
;;; 1) CL constants cannot be overriden in a lexical binding:
;;;     (defconstant myvar 1)
;;;     (let ((myvar ...)) ;; causes an error
;;;       ...)
;;;    Also, we need to be able set a new value at runtime.  
;;; 2) CL dynamic variables have context-dependent values. E.g., 
;;;    (defvar i #C(0 1)) ;; the imaginary number
;;;    (defun myfun (n)
;;;      (format t "~S " (* i n))) ;; print the complex conjugate of n
;;;    (dotimes (i 100) ;; innocently use i as value iterator
;;;      (myfun i)) 
;;;    expecting: 0 #C(0 1) #C(0 2) #C(0 3)...
;;;    but get:   0 1 4 9 ...



;;; Globals are implemented as symbol-macros.  
;;;
;;; The global value is stored in a symbol, called a hidden-place
;;; Placeholders are stored in the package held by +global-package+
;;;
;;; The symbol-macro is a function which returns the value in the placeholder.
;;; e.g.,
;;;
;;; (define g 'GLOBAL-VALUE) => GLOBAL-VALUE
;;; (macroexpand 'g) => (B.GLOBALS::B*G)
;;;
;;; (defun test-global ()
;;;   (princ g *standard-output*)) 
;;;
;;; (defun test-lexical ()
;;;   (let ((g 'lexical-value)) ;; no error
;;;      (princ g *standard-output*))) 
;;;
;;; (defun test-set-global ()
;;;    (setf g 'test-value))
;;;
;;; (test-global) => "GLOBAL-VALUE"
;;; (test-lexical) => "LEXICAL-VALUE"
;;;
;;; (test-set-global) => error undefined function (SETF B.GLOBAL::B*G)

;;; $Id: global.lisp,v 1.5 2008/09/02 14:58:10 amallavarapu Exp $

(in-package b)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;;
;;; Top-level-forms -
;;; 

;;;
;;; GLOBAL PARAM - expands to a hidden param
;;;
(defmacro define-global-parameter (s form)
  `(_define-global-symbol defparameter ,s ,form))

(port:setup-indent 'define-global-parameter 1 2 4)

;;; GLOBAL VAR - expands to a hidden VAR
(defmacro define-global-var (s form)
  `(_define-global-symbol defvar ,s ,form))

(port:setup-indent 'define-global-var 1 2 4)

;;;
;;; GLOBAL-CONST - a global constant value 
;;;
(defmacro define-global-const (s form)
  `(_define-global-symbol defconstant ,s ,form))


(defun clear-global (s)
  (whenit (find-symbol (make-global-place-name s) +global-package+)
    (portable:destroy-symbol it :undefinep t :forget-dspec-p t)
    (unintern it +global-package+)))

(port:setup-indent 'define-global-const 1 2 4)

(defun reload-global-definition (symbol place ctor)
  (declare (ignorable symbol))
  (when (fboundp ctor)
    (let ((val (funcall ctor)))
      (when place (setf (symbol-value place) val)))))

(defun reload-global-definition-fn (symbol)
  (let ((place (make-global-place-symbol symbol))
        (ctor  (make-global-ctor-symbol symbol)))
    (lambda () 
      (reload-global-definition symbol place ctor))))

(defun clear-global-definition-fn (symbol)
  (let ((place (make-global-place-symbol symbol))
        (ctor  (make-global-ctor-symbol symbol)))
    (lambda ()
      (unintern symbol)
      (unintern place)
      (fmakunbound ctor)
      (unintern ctor))))

;;;; (defun install-global-definition (symbol place ctor-sym)
;;;;   "Helper macro - runs the ctor function and reports the definition to the definitoin stack"
;;;;   (let ((reload-fn (when ctor-sym 
;;;;                      (lambda () 
;;;;                        (let ((val (funcall ctor-sym)))
;;;;                          (if place (setf (symbol-value place) val))
;;;;                          val))))
;;;;         (clear-fn  (lambda ()
;;;;                      (unintern symbol)
;;;;                      (if place (unintern place))
;;;;                      t)))
;;;;     (report-definition `(define ,symbol) reload-fn clear-fn nil)
;;;;     (when reload-fn
;;;;       (funcall reload-fn))))

;;; GLOBAL - defines a symbol which expands to a function which accesses a var
(defmacro define-global (s &rest body)
  (let* ((last-form        (first (last body)))
         (type             (if (the-expr-p last-form)  (the-expr-type last-form)))
         (ctor-sym         (make-global-ctor-symbol s))
         (place-sym        (make-global-place-symbol s))
         (ctor-def         (compute-global-ctor-code ctor-sym body))
         (place-def        (compute-global-place-code place-sym type))
         (expansion        `(,place-sym))
         (typed-expansion  (if type
                               `(the ,type ,expansion) expansion)))
    
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,s ,typed-expansion)
       ,@place-def
       (locally (declare (special ,place-sym))
         ,ctor-def
         (reload-global-definition ',s ',place-sym ',ctor-sym)
         ',s))))


(port:setup-indent 'define-global 1 2 4)

;;;; (defmacro define-global-block (&rest body)
;;;;   "A defines a resettable block of code without defining a symbol."
;;;;   (let* ((ctor-sym (make-global-place-symbol body))
;;;;          (ctor-def (compute-global-ctor-code ctor-sym body))
;;;;          (gbi      (gensym "GBI")))
;;;;     `(eval-when (:compile-toplevel :load-toplevel :execute)
;;;;        ,ctor-def
;;;;        (install-global-definition ',ctor-sym nil ',ctor-sym))))
;;;; ;       (let ((,gbi (record-global-block-info ',ctor-sym nil ',ctor-sym)))
;;;; ;         (reset-global-block-info ,gbi)))))
;;;;      


;;; PREDEFINE - to wrap code which needs to refer to a global before it has been defined
;;;           - the :class key is synonymous with :const 
;;;           - the :object key is synonymous with :form
;;; Usage:
;;;        (predefine ( [:object [symbol [( [symbol | (symbol type)]*] )]
;;;                     [:var    [symbol [( [symbol | (symbol type)]*] )]
;;;                     [:const  [symbol [( [symbol | (symbol type)]*] )]
;;;           [code])
;;; E.g.,
;;;        (predefine (:const MyConst :var ((Myvar string)) :object (MyObject))
;;;          (setf MyVar 100) ; will cause a compile error because type=string
;;;          (setf Myobject 1) ; will cause an error because it is a object
;;;          (format t "~S" MyConst) ;; prints value of MyConst
;;;          (let ((myconst 1000)) ; no error caused here, since this is a global
;;;            (format t "~S" MyConst) ;; prints 1000
;;;
(defmacro predefine ((&key global const var form object class) &body body)
  "Enables references to symbols which have yet to be defined (e.g., with define, defcon, defrule)"
  (labels ((symbol-def-symbol (s)
             (if (consp s) (first s) s))
           (symbol-def-type (s)
             (if (consp s) (second s)))
           (typed-expansion (s expansion)
             (let ((type (symbol-def-type s)))
               (if type `(the ,type ,expansion)
                 expansion)))
           (macrolet-spec (s expansion)
             `(,(symbol-def-symbol s) ,(typed-expansion s expansion)))
           (object-expansion (s)
             (let ((exp `(,(make-global-place-symbol (symbol-def-symbol s)))))
               (macrolet-spec s exp)))
           (var-expansion (s)
             (let ((exp `(symbol-value ',(make-global-place-symbol s))))
               (macrolet-spec s exp)))
           (class-expansion (s)
             (let ((exp `(the standard-class (symbol-value ',(make-global-place-symbol s)))))
               (macrolet-spec s exp))))
    (handler-case 
        (let* ((mlet-specs (append (mapcar #'var-expansion 
                                           (append (ensure-list var) (ensure-list const) (ensure-list global)))
                                   (mapcar #'class-expansion
                                           (append  (ensure-list class)))
                                   (mapcar #'object-expansion 
                                           (append (ensure-list form) (ensure-list object)))))
               (special-symbols    (mapcar (lambda (spec) (make-global-place-symbol (car spec)))
                                           (append (ensure-list var) (ensure-list form)
                                                   (ensure-list object)))))
          `(locally
             (declare (special ,@special-symbols))
             (symbol-macrolet ,mlet-specs
               ,@body)))
      (error (e) (error "During ~S: ~A" form  e)))))

;;;
;;; GLOBAL - manipulation & predicates
;;;
(defmacro with-global-expansion ((gsymbol &optional exp expandedp check env) &body body)
  "Expands GSYMBOL, placing the result in EXP and a boolean indicating success in EXPANDEDP.  
ERRORP controls whether a error message is produced if EXPANDEDP is false. 
Check may be NIL    body is always executed
             T      body executed only if gsymbol expands into a valid form
             :ERROR an error is thrown if the gsymbol doesn't expand to a valid form."
  (let ((expandedp (or expandedp (gensym "expandedp")))
        (exp       (or exp (gensym "expansion"))))
    `(multiple-value-bind (,exp ,expandedp)
         (macroexpand ,gsymbol ,env)
       (cond
        ((valid-global-expansion-check ,exp ,expandedp ,check)
         ,@body)
        ((eq ,check :error)
         (error "Cannot access global-value of ~S - no value defined." ,gsymbol))))))

(defun valid-global-expansion-check (exp expandedp check)
  (or (not check)
      (and expandedp
           (let ((red-form (the-expr-reduce-to-form exp)))
             (typecase red-form
               (cons    (and (eq (length red-form) 1)
                             (place-var-p (first red-form))))
               (symbol  (place-symbol-p red-form)))))))

(defun global-value (s &optional (errorp t) (env nil))
  (with-global-expansion (s exp nil (if errorp :error t) env)
    (let ((reduced (the-expr-reduce-to-form exp)))
      (symbol-value
       (typecase reduced
        (cons   (first reduced))
        (symbol reduced))))))

(defun global-var-p (s)
  (with-global-expansion (s exp exp?)
    (and exp?
         (place-var-p (the-expr-reduce-to-form exp)))))

(defun global-const-p (s)
  (with-global-expansion (s exp exp?)
    (and exp?
         (constantp (the-expr-reduce-to-form exp)))))

(defun global-block-p (s)
  (with-global-expansion (s exp exp?)
    (let ((exp (the-expr-reduce-to-form exp)))
      (and exp? 
           (consp exp)
           (eq (length exp) 1)
           (place-var-p (first exp))))))
                     
(defun global-read-only-p (s)
  "True for global-consts and global-blocks"
  (or (global-block-p s)
      (global-const-p s)))

(defun globalp (s &optional env)
  (and (symbolp s)
       (with-global-expansion (s exp nil t env) 
         t)))


;;;
;;; Support macro for define-global-var and define-global-const
;;;
(defmacro _define-global-symbol (place-def s form)
  (let* ((place        (make-global-place-symbol s))
         (exp-form     (macroexpand form))
         (type         (cond ((the-expr-p exp-form) (the-expr-type exp-form))
                             ((and (eq place-def 'defconstant)
                                   (constantp exp-form) (type-of exp-form)))))
         (typed-place  (if type `(the ,type ,place) place)))
    `(eval-when (:compile-toplevel :load-toplevel :Execute)
       ,@(if type `((declaim (type ,type ,place))))
       (eval-when (:compile-toplevel :load-toplevel :execute) (,place-def ,place ,form))
       (define-symbol-macro ,s ,typed-place)
       (reload-global-definition ',s ',place nil)
       ',s)))

;;;
;;; Support functions for define-global-block
;;;
(defun compute-global-ctor-code (ctor-sym body)
  `(defun ,ctor-sym () ,@body))

(defun compute-global-place-code (place type)  
  (let ((type-decl (if type `((declaim (ftype (function () ,type) ,place)
                                       (type ,type ,place))))))
    `(;(declaim (inline ,place)) ;; inlines the function (i.e., place is accessed without fncall overhead)
      ,@type-decl
      (defvar ,place)
      (defun ,place () 
        (declare (optimize (speed 3)) 
                 (special ,place)) 
        ,place))))
     

;;;; ;;;
;;;; ;;; GLOBAL-INFO structure - used for global vars and global consts
;;;; ;;;
;;;; (defstruct (global-info (:conc-name global-info-)
;;;;                         (:constructor global-info (symbol place)))
;;;;   symbol place)

;;;; (defun record-global-info (s value)
;;;;   (remove-global-info s)
;;;;   (let ((gbi (global-info s value)))
;;;;     (push gbi *old-definition-stack*)
;;;;     gbi))
;;;;   
;;;; (defun remove-global-info (s &optional warning)
;;;;   (let ((existing (find s *old-definition-stack* :key #'global-info-symbol)))
;;;;     (when existing 
;;;;       (if warning (warn warning s))
;;;;       (setf *old-definition-stack* (remove existing *old-definition-stack*)))))


;;;; ;;;
;;;; ;;; global-block-INFO structure
;;;; ;;;
;;;; (defstruct (global-block-info (:include global-info)
;;;;                         (:conc-name global-block-info-)
;;;;                         (:constructor global-block-info (symbol place fn)))
;;;;   fn)

;;;; (defun record-global-block-info (s place fn)
;;;;   "Maintains the symbol in the order it was defined.  Fn is a function which evaluates the form.
;;;; Returns the global-block-info record, a cons of s, fn."
;;;;   (remove-global-info s); "Redefining global form ~S.  Reset order may have changed.")    
;;;;   (let ((gbi (global-block-info s place fn)))
;;;;     (push gbi *old-definition-stack*)
;;;;     gbi))

;;;; (defun reset-global-block-info (gbi)
;;;;   (let* ((place  (global-block-info-place gbi))
;;;;          (fn     (global-block-info-fn gbi))
;;;;          (val    (funcall fn)))

;;;;     (when place (setf (symbol-value place) val))))

;;;; (defun reset-global-blocks ()
;;;;   (loop for gi in (reverse *old-definition-stack*)
;;;;         when (typep gi 'global-block-info)
;;;;         sum  (progn (reset-global-block-info gi) 1)))


;;;
;;; GLOBAL PLACE SYMBOLS - where the value of a global-block-INFO is stored 
;;;
(defun make-global-place-name (s &rest suffixes)
  (let ((pkg  (if (symbolp s) (symbol-package s) *package*)))
    (apply #'mkstr (package-name pkg)
           ".<" s ">" suffixes)))

(defun make-global-place-symbol (s &rest suffixes)
  (intern (apply #'make-global-place-name s suffixes)
          +global-package+))

(defun make-global-ctor-symbol (s)
  (intern (mkstr (make-global-place-name s) "-CTOR") +global-package+))

(defun place-symbol-p (s)
  (and (symbolp s)
       (eq (symbol-package s)
           +global-package+)))

(defun place-var-p (s)
  (and (place-symbol-p s)
       (not (constantp s))))

(defun place-const-p (s)
  (and (place-symbol-p s)
       (constantp s)))

(defun initialize-global-value ()
  "Clears the global-value package"
  (do-symbols (s 'global-value)
    (unless (eq (string<= "B." (symbol-name s)) 2)
      (portable:destroy-symbol s :undefinep t)
      (unintern s 'global-value))))

;;;; ;;;
;;;; ;;; FOR CLEARING GLOBAL DEFINITIONS:
;;;; ;;; 

;;;; ;; GLOBAL-MARKER   - names a place in the stack of global definitions
;;;; (defconstant +global-marker+ (make-symbol "GLOBAL-MARKER"))

;;;; (defun mark-global-definitions (name)
;;;;   "Marks a point in the stack of global definitions which may be named by clear-globals.  Clear-definitions will clear definitions (most recent first) until the marked point."
;;;;   (flet ((make-global-marker (name)
;;;;            (global-info +global-marker+ name)))
;;;;     
;;;;     (push (make-global-marker name) *old-definition-stack*)))


;;;; (defun clear-globals (marker-name clear-marker)
;;;;   "Clear globals until the marker named NAME.  If that marker with that name does't exist, an error is thrown."
;;;;   (flet ((global-marker-found (gi)
;;;;            (or (null gi)
;;;;                (and (eq (global-info-symbol gi) +global-marker+)
;;;;                     (equal (global-info-place gi) marker-name)))))

;;;;     (let ((exists (or (eq t marker-name) (find-if #'global-marker-found *old-definition-stack*))))
;;;;       (cond
;;;;        (exists (loop with gi = (pop *old-definition-stack*)
;;;;                      while (not (global-marker-found gi))
;;;;                      do (clear-using-global-info gi)
;;;;                         (setf gi (pop *old-definition-stack*))
;;;;                      sum 1
;;;;                      finally (unless (or clear-marker 
;;;;                                          (not (global-marker-found gi)))
;;;;                                (push gi *old-definition-stack*))))

;;;;        (t      (error "Cannot clear global definitions: marker ~S doesn't exist." marker-name))))))

;;;; (defun clear-using-global-info (gi)
;;;;   ;; until I figure out a better way to undefine symbol-macros:
;;;;   (unintern (global-info-symbol gi))
;;;;   (unintern (global-info-place gi)))

;;;; (defun get-global-markers ()
;;;;   "Returns a list of global marker names in most-recent to least-recent order."
;;;;   (remove-if (lambda (gi)
;;;;                (#!eq (global-info-symbol gi)
;;;;                     +global-marker+))
;;;;              *old-definition-stack*))
)
    
