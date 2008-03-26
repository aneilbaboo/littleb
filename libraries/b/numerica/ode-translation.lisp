;;; This file is part of little b.

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


;;; File: b/matlab:ode-translation
;;; Description: 


(in-package #I@FILE)

(include-declaration :expose-symbols (create-numerica-model)
                     :use-packages (mallavar-utility portable))                     

(include (@LIBRARY/math
          @LIBRARY/units/dimensionalization 
          @LIBRARY/math/ode-var))



(include-documentation 
  :description "Provides capability for translating little b temporal/reference/derived-vars to Numerica integration code."
  :organization "Harvard Medical School"
  :authors ("Aneil Mallavarapu"))

(define-var *numerica-model* ())
(defvar  *numerica-rate-string-max-length* 2000
  "Part of a horrendous kludge to deal with a numerica bug")

(define-function create-numerica-model (name &key 
                                        (vars (progn (format t "~&; Collecting variables...~%") (query ode-var)))
                                        (ode-comments t)
                                        (display-vars t)
                                        (overwrite :query)
                                        (abstol nil) 
                                        (reltol nil)
                                        (gauss-value :mean) ; may be mean or random
                                        (base-units (compute-default-base-units-from-vars vars)))
  (format t "~&; Creating Numerica model...~%")
  (let+ ((*print-pretty*          nil)
         ((name path)             (parse-model-name name))
         (nm                      (make-numerica-model name
                                                       :path path
                                                       :ode-vars vars
                                                       :ode-comments-p ode-comments
                                                       :reltol reltol
                                                       :abstol abstol
                                                       :gauss-value gauss-value
                                                       :display-vars (order-vars (if (eq t display-vars) vars
                                                                                   display-vars))
                                                       :base-units  base-units))
         (pathname                (numerica-file-path (numerica-model-name nm) path)))

        (when (ok-to-overwrite overwrite pathname)
          (unless vars
            (warn "*** No ode vars selected ***~%"))
          (format t "~&; Computing Numerica code...~%")
          (with-open-file (stream pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
            (let ((*math-print-function*   (compute-numerica-math-print-function nm)))
              (write-numerica-code nm stream))))
        (setf *numerica-model* nm)))


;;;; (defun refresh-parameters (&optional (model *model*))
;;;;   "Rewrites the parameters/initial conditions of a model (default is *model*) to the model's initfile."  
;;;;   (let* ((name (numerica-model-init-script-name model))
;;;;          (path (numerica-model-path model))
;;;;          (init-path (numerica-file-path name path)))
;;;;     (with-open-file (file init-path :direction :output)
;;;;       (write-init-script file model))
;;;;     (format t "New parameters file (~A.~A) written to ~A."
;;;;             (pathname-name init-path)
;;;;             (pathname-type init-path)
;;;;             path)))

(defun make-safe-file-name (o)
  (substitute-if-not #\_ #'alphanumericp
                     (string-downcase o)))

(defun numerica-file-path (name path)
  (if (streamp path) path  ; a little hack to allow redirection of output from files to a single stream
    (normalize-pathname (merge-pathnames (mkstr name ".jac") path))))

(defun compute-indicies (varlist &optional (model *model*))
  (assert model (model)
    "No current model.  Please provide a model name.")
  (with-print-context t
    (format t "~&~A({'~A'~{,'~A'~}})"
            (numerica-model-var-indicies-name model)
            (first varlist) (rest varlist))))

(defun order-vars (vars)
  (format t "~&; Sorting variables...~%")
  (sort (copy-list vars)  ; must copy the list! Allegro destructively modifies the input list (removing elements!!!)
        #'string<
        :key (lambda (o) 
               (format nil "~(~A~)" o))))

(defun query-time-vars (&key prefer (base-set (query ode-var)))
  (compute-ode-vars base-set prefer))

(defun write-numerica-code (nm file)
  (write-declare-block nm file)
  (write-model-block nm file)
  (write-simulation-block nm file)
  
  (format t "~&; The following file has been written: ~A~%"
          (enough-namestring file)))

;;;
;;; helper functions which write numerica code to a stream:
;;;

(defun write-declare-block (nm file)
  (declare (ignore nm))
  (format file "DECLARE~%~
                ~2TTYPE~%~
                ~4TConcentration = 1 : -1e-6 : 1E20~%~
                END~%"))

(defun split-rate-string-equally (str &optional (max-char-size *numerica-rate-string-max-length*))
  (let ((term-breaks (sort (nconc (positions #\+ str)
                                  (positions #\- str))
                           #'<)))
    (loop with cur-start-pos = 0
          for last-term-pos = 0 then term-pos
          for term-pos in term-breaks
          for expr-len = (- term-pos cur-start-pos)
          when (> expr-len max-char-size)
          collect (subseq str cur-start-pos last-term-pos) into parts
          and do (setf cur-start-pos last-term-pos)
          finally 
          (setf parts (nconc parts (if (> last-term-pos cur-start-pos)
                                       (list (subseq str cur-start-pos last-term-pos)))))
          (return parts))))
          
(defun write-model-block (nm file)
  (let* ((ode-vars  (numerica-model-ode-vars nm))
         (allvars (query var))
         (klen-pos  nil))
    (labels ((rate-str-without-outer-parens (rate)
               (let ((rate-str (format nil "~S" rate)))
                 (with-dimensionless-math (numerica-model-base-units nm)
                   (if (char= (char rate-str 0) #\()
                       (subseq rate-str
                               1 (1- (length rate-str)))
                     rate-str))))
             (write-ode-fn (i rate)
               (let ((rate-strs (split-rate-string-equally (rate-str-without-outer-parens rate))))                  
                 (princ #\.)
                 (cond
                  ((> (length rate-strs) 1)
                   (loop for rate-str in rate-strs
                         for p = 1 then (1+ p)
                         do (format file "~%~4Tpart~S := ~A;" p rate-str)
                         collect p into parts
                         finally (format file "~%~4T$c(~S) = ~{part~S~^+~};" i parts)))
                  (t (format file "~%~4T$c(~S) = ~A;" i (first rate-strs)))))))

    (format t "~&; Writing model block")
    (format file "MODEL ~A~%~
                  ~2TPARAMETER~%~
                    ~4Tk as ARRAY("
            (numerica-model-model-name nm))

    ;; store the location of the k array length so we can set it later...
    (setf klen-pos (file-position file))

    (format file "~A) OF REAL~%~
                  ~2TVARIABLE~%~
                    ~4Tc as ARRAY(~S) of Concentration~%~
                  ~2TEQUATION"
            (map 'string (constantly #\space) (format nil "~A" (length allvars)))
            (length ode-vars))

    ;; write the rate fns:
    (loop for v in ode-vars
          for i = 1 then (1+ i)
          for rate = (compute-ode-expression (nth (1- i) (numerica-model-ode-rates nm)) nm)
          do  (with-print-context t 
                (princ #\.)
                ;; write comment
                (if (numerica-model-ode-comments-p nm)
                    (b-format file "~%~4T# ~A = ~A " v rate))
                ;; write eqn
                (write-ode-fn i rate)))

    ;; now, we've determine the actual # kvars ...
    ;; go back and scribble at klen-pos
    (let ((end (file-position file)))
      (file-position file klen-pos)
      (princ (numerica-model-kvar-count nm) file)
      (file-position file end))
    (format file "~%END~%~%"))))


(defun compute-display-var-indicies (nm)
  (remove nil (mapcar (lambda (i) (ode-var-index i nm)) (numerica-model-display-vars nm))))

(defun write-simulation-block (nm file)
  (let ((vars (numerica-model-ode-vars nm)))
    (format t "~&;  Writing simulation block")
    (format file "SIMULATION ~A~%~
                  ~2TOPTIONS~%~
                    ~4TDYNAMIC_REPORTING_INTERVAL := 1 ;~%~
                    ~4TNORMALIZED_SENS := TRUE ; ~%~
                    ~4TINIT_PRINT_LEVEL := 0 ; ~%~
                    ~4TREINIT_PRINT_LEVEL := 0 ; ~%~
                    ~4TDYNAMIC_PRINT_LEVEL := 0 ; ~%~
                  ~2TPARAMETER ~%~
                    ~4TL0 AS REAL # Used to control input function L from script~%~
                  ~2TUNIT~%~
                    ~4Tm AS ~A~%~
                  ~2TSENSITIVITY~%~
                    ~4Tm.k~%~
                  ~2TSET~%~
                    ~4TL0 := 1e-9 ;~%"
            (numerica-model-simulation-name nm)
            (numerica-model-model-name nm))
    ;; write the parameters:
    (write-kvars nm file)

    (format t "~&; Writing initial conditions")
    ;; write the initial conditions:
    (format file "~2TINITIAL~%~
                    ~4TWITHIN m DO")
    (loop for v in vars
          for i = 1 then (1+ i)
          do (with-print-context t
               (princ #\.)
               (let* ((base-units (numerica-model-base-units nm))
                      (the-unit v.dimension.(calculate-unit base-units)))
                 (with-dimensionless-math 
                  base-units
                  (b-format file "~%~6Tc(~A) = ~A ; #" i (print-math v.t0 nil)))
                 (b-format file " [~A] ~A~@[ in ~A~]" i v (unless (eq null-unit the-unit) the-unit)))))

    (format file "~%~4TEND~%~
                  ~2TSCHEDULE~%~
                  ~4TCONTINUE FOR 30~%~
                  END~%")))


(defun write-kvars (nm file)
  (let* ((kvars-table (numerica-model-kvars nm))
         (kvars  (make-array (1- (hash-table-count kvars-table)))))
    (loop for kvar being the hash-keys of kvars-table
          for i being the hash-values of kvars-table
          unless (eq kvar :last-index)
          do (setf (svref kvars (1- i)) kvar))
    (format t "~&; &Writing parameters")
    (format file "~2TWITHIN m DO")
    (with-print-context t
      (loop for k across kvars 
            for i = 1 then (1+ i)
            for k-unit = k.dimension.(calculate-unit (numerica-model-base-units nm))
            for value = k.value
            do (princ #\.)
            if (math-expression-p value) ; calculate after other kvars are set
            collect (list i k) into dependent-vars
            else
            do  (with-dimensionless-math (numerica-model-base-units nm)
                  (format file "~%~6Tk(~a) := ~a; #" i (print-math value nil)))
            (b-format file " ~A ~@[in ~A~] "
                      k (unless (eq null-unit k-unit) k-unit))
            finally 
            ;; write any dependent vars:
            (loop for (i k) in dependent-vars
                  do (with-dimensionless-math (numerica-model-base-units nm)
                       (format file "~%~k(~A) := ~S ;" i k.value)))))
    (format file "~%~2TEND~%")))
  

(defun escape-numerica-formatting-chars (str)
  (find-and-replace-all
   "\\" "\\\\"
   (find-and-replace-all
    "^" "\\^"
    (find-and-replace-all "_" "\\_" str))))
  
(defun write-descriptions (file nm)
  (flet ((write-var (v stream)
           (with-print-context t
             (let ((d v.dimension))
               (m-format stream
                         "\'~A~@[ (~S)~]'" 
                         v (unless (eq d null-dimension)
                             d.(calculate-unit (numerica-model-base-units nm))))))))
    (let ((vars (numerica-model-ode-vars nm)))
      (format file "~A=char( ...~%" (numerica-model-descriptions-var-name nm))
      (when vars
        (write-var (first vars) file)
        (loop for v in (rest vars)
              for i = 1 then (1+ i)
              do (format file ",... % [~S]~% " i)
              (write-var v file))
        (format file ");~%~%")))))


(defun m-format (stream str &rest args)
  "Ensures any numerica formatting chars are escaped"
  (princ (escape-numerica-formatting-chars (format nil "~?" str args)) stream))

(defun b-format (stream string &rest args)
  "Prints using b print style, creating strings suitable for numerica"
  (let ((*math-print-function* 'default-math-printer))
    (apply #'format stream string args)))

  

;;;
;;; FNs for treating streams as pathnames 
(defun carefully-get-filename (f)
  (handler-case (pathname-name f) 
    (error () "unknown")))

(defun carefully-get-filetype (f)
  (handler-case (pathname-type f)
    (error () "?")))

(defun kvar-filename (stream)
  "returns the name of the ...."
  (format nil "~S_k" (carefully-get-filename stream)))

;;;;
;;;;   NUMERICA-MODEL
;;;;
;;;;    structure which stores information about how NUMERICA code is to be generated
;;;;

;;
;; the numerica-model structure stores information about how NUMERICA code is to be generated
;;
(defstruct (numerica-model (:constructor _make-numerica-model 
                          (_name path ode-vars ode-rates base-units gauss-value kvars display-vars ode-comments-p)))
  _name ; string indicating the name of the model
  path ; the path in which files for this model are stored
  ode-vars ; an ordered list of n ode-vars/derived-vars which map to y(1)...y(n) in the numerica ODE system
  ode-rates ; the .rates of the ode-vars
  base-units ; units (of base dimensions only) which all quantities are converted to
  gauss-value ; either :mean or :random - controls how gaussians are sampled G.mean or G.random-value
  kvars ; ordered list of vars which represent parameters (as opposed to ode-vars)
  display-vars ; a list of ode-vars/derived-vars which should be displayed in numerica
  ode-comments-p)  ; print comments for ode-models
 

(defmethod print-object ((nm numerica-model) stream)
  (print-unreadable-object (nm stream :type t :identity t)
    (format stream "~A (~A vars)" (numerica-model-name nm)
            (length (numerica-model-ode-vars nm)))))

(defun make-numerica-model (model-name 
                          &key 
                          (ode-comments-p t)
                          (path nil)
                          (ode-vars (query [ode-var ?])) 
                          (base-units nil) 
                          (abstol nil)
                          (reltol nil)
                          (gauss-value :mean) 
                          (display-vars nil))
  (let* ((rates (progn (format t "~&; Computing rates...~%") (mapcar ?.rate ode-vars)))
         (kvars (make-hash-table))) ; (order-vars (compute-kvars rates))))
    (setf (gethash :last-index kvars) 0)
    (_make-numerica-model model-name path ode-vars rates base-units gauss-value kvars display-vars ode-comments-p)))

(defun numerica-model-name (nm)
  (etypecase nm
    (string nm)
    (numerica-model (numerica-model-_name nm))))

(defun numerica-model-init-script-name (nm)
  (mkstr (numerica-model-name nm) "_init"))
(defun numerica-model-kvars-name (nm)
  (mkstr (numerica-model-name nm) "_k"))
(defun numerica-model-descriptions-var-name (nm)
  (mkstr (numerica-model-name nm) "_descriptions"))
(defun numerica-model-simulation-name (nm)
  (mkstr (numerica-model-name nm) "_simulation"))
(defun numerica-model-model-name (nm)
  (mkstr (numerica-model-name nm) "_model"))
(defun numerica-model-var-indicies-name (nm)
  (mkstr (numerica-model-name nm) "_vars"))
(defun numerica-model-kvar-count (nm)
  (1- (hash-table-count (numerica-model-kvars nm))))
;;;
;;; functions for computing default base units
;;;
(defun compute-nearest-base-unit-quantity (q)
  (let ((dim (dimension-of q)))
    (typecase dim
      (base-dimension     (values q dim))
      (derived-dimension  (let ((bdim (b/math::dimension-element-base
                                       (find 1 dim.base-dimensions 
                                             :key #'b/math::dimension-element-power))))
                            (when bdim
                              (compute-nearest-base-unit-quantity (quantity q.base-magnitude bdim))))))))

(defun compute-default-base-units-from-vars (vars)
  (let ((min-base-unit-quantities (make-hash-table))) ; key=base dimension 
                                                      ; val=lowest non-zero t0 base quantity in dimension
    (loop for var in vars
          for t0 = var.t0
          for evaledt0 = (typecase  t0
                           (math-expression t0.eval-vars)
                           (var t0.value)
                           (t t0))
          for (q dim) = (when (not (zero-numeric-p evaledt0))
                          (multiple-value-list (compute-nearest-base-unit-quantity evaledt0)))
          for existing = (gethash dim min-base-unit-quantities)
          unless (or (null q) (and existing {q > existing}))
          do (setf (gethash dim min-base-unit-quantities) q))
  
    (loop for v being the hash-values in min-base-unit-quantities
          collect  v.nearest-unit)))
  


(defun compute-ode-vars (vars preferred-vars)
  "Where vars is a list of ode-vars or derived-vars, and preferred-vars is a list of derived-vars.
Returns a list of vars which represent the ode system.  (E.g., to select derived-vars describing
concentration rather than ode-vars describing moles).  If one of vars is a normal-base of a 
preferred-var, then the preferred-var is returned, otherwise the ode-var is used."
  (labels ((ode-var-from-var (v)             
             (or (find v preferred-vars :key ?.normal-base)
                 v)))
    (if preferred-vars
      (mapcar #'ode-var-from-var vars)
      vars)))

(defun kvar-p (v)
  (or (reference-var-p v)
      (and (derived-var-p v)
           (reference-var-p v.normal-base))))

(defun ode-var-index (v nm)
  (compute-var-index v (numerica-model-ode-vars nm)))

(defun kvar-index (v nm)
  (let ((kvars (numerica-model-kvars nm)))
    (or (gethash v kvars)
        (setf (gethash v kvars)
              (incf (gethash :last-index kvars))))))

;  (compute-var-index v (numerica-model-kvars nm)))


(defun compute-var-index (v vars)
  (let ((pos  (position v vars)))
    (if pos (1+ pos)
        (error "Unable to find ~S in variables ~S" v vars))))


(defgeneric compute-ode-expression (e nm)
  (:documentation "Given a var, derived-var or expression, returns an expression in terms of
the vars in (numerica-model-ode-vars nm).  Since DERIVED-VARs can be expressed in terms of reference-vars or
ODE-VARs, the system must decide which form to prefer.  By default normal-vars are prefered 
(reference-vars and ODE-VARs) - if a DERIVED-VAR is listed in (codefinfo-ode-vars nm), then it is prefered."))

(defmethod compute-ode-expression ((e reference-var) nm)
  (declare (ignorable nm)) e)
  
(defmethod compute-ode-expression ((e ode-var) nm)
  (or (find e (numerica-model-ode-vars nm))
      (let ((derived-var (find e (numerica-model-ode-vars nm) :key ?.normal-base)))
        (assert (derived-var-p derived-var) () "Cannot compute ode-expression of ~S" e)
        derived-var.normal-inverse-fn)))

(defmethod compute-ode-expression ((e derived-var) nm)
  (or (find e (numerica-model-ode-vars nm))
      (if (find e.normal-base (numerica-model-ode-vars nm))
          e.normal-fn)
      (assert nil () "Cannot compute ode-expression of ~S" e)))

(defmethod compute-ode-expression ((e math-expression) nm)
  e.(map (lambda (v) (compute-ode-expression v nm))))

(defmethod compute-ode-expression (o nm) 
  (declare (ignorable nm)) o)

(defun compute-numerica-math-print-function (nm)
  (let ((old-print-fn *math-print-function*))
    (lambda (object stream &optional (lhs-op nil))
      (typecase object
        (var       (if (kvar-p object) (write-kvar-code object nm stream)
                            (write-ode-var-code object nm stream)))

        (quantity  (with-dimensionless-math (numerica-model-base-units nm)
                     (funcall old-print-fn object stream lhs-op)))

        (real      (funcall old-print-fn object stream lhs-op))

        (complex (format stream "(~A~[+~]~Ai)" 
                         (realpart object) 
                         (if (minusp (imagpart object)) 1 0)
                         (imagpart object)))

        (number  (funcall old-print-fn (coerce object 'float) stream lhs-op))

        (math-expression (funcall old-print-fn object stream lhs-op))
       
        (t
         (case object
           (*     (princ #\* stream))
           (+     (princ #\+ stream))
           (/     (princ #\/ stream))
           (-     (princ #\- stream))
           (^     (princ #\^ stream))
           (#\(   (princ #\( stream))
           (#\)   (princ #\) stream))
           (t     (prin1 object stream))))))));(b-error "Unable to print numerica code for ~S." object))))))))

(defun write-ode-var-code (v nm stream)
  (format stream "c(~A)" ; note: c is a local variable in the numerica ode function
          (ode-var-index v nm)))

(defun write-kvar-code (v nm stream)
  (format stream "k(~S)" 
          ;(numerica-model-kvars-name nm)
          (kvar-index v nm)))

;;;;
;;;; file helper functions
;;;;

(defun parse-model-name (p)
  "Breaks a pathname into 2 parts, the file name and directory, and checks that
   the name is valid for a numerica function."
  (let* ((p    (merge-pathnames p (or *load-truename* "")))
         (name (pathname-name p))
         (path (make-pathname :name nil :type nil 
                              :defaults p)))
    (unless (eq (first (pathname-directory path)) :absolute)
      (warn "Output folder was not provided.  Files will be saved to ~A.~%~%~%"  
            (truename path)))

    (check-numerica-function-name name)
    (values name path)))
                 
(defun check-numerica-function-name (str)
  (unless 
      (and (> (length str) 0)
           (valid-numerica-function-first-char-p (elt str 0))
           (or (< (length str) 2)
               (every #'valid-numerica-function-next-char-p
                      (subseq str 1))))
    (error "~S is not a valid Numerica function name." str)))

(defun valid-numerica-function-next-char-p (c)
  (or (alphanumericp c)
      (char= c #\_)))

(defun valid-numerica-function-first-char-p (c)
  (or (alpha-char-p c)
      (char= c #\_)))

(defun ok-to-overwrite (flag &rest paths)
  (or (not (some #'probe-file paths))
      (case flag
        (:query (portable:prompt-for-yes-or-no
                 "One or more of the following files may need to be deleted: ~{~A~^, ~}.~%Proceed?" 
                 (mapcar #'enough-namestring paths)))
        ((nil)  nil)
        (:error (error "One or more of the following files already exists: ~{~A~^, ~}."
                       (mapcar #'enough-namestring paths)))
        (t      t))))







;;; this almost works: sometimes all the
;;;    t0 values are math-expressions,
;;;    this means we must de-ref all the variables.
;;;; (defun guess-error-tolerance-from-vars (ci)
;;;;   (flet ((lowest-non-zero (vars val-key)
;;;;            (min (mapcar (compose #'abs #'dimensionless)
;;;;                         (remove-if (lambda (o)
;;;;                                      (or (not (numericp o))
;;;;                                          (zero-numeric-p o)))
;;;;                                    (mapcar ?.t0 vars))))))
;;;;     (with-dimensionless-math (numerica-model-base-units ci)
;;;;       (let ((low-t0     (lowest-non-zero (numerica-model-ode-vars ci) ?.t0))
;;;;             (low-kvar   (lowest-non-zero (numerica-model-kvars ci) ?.value)))
;;;;         {low-t0 low-kvar}))))










