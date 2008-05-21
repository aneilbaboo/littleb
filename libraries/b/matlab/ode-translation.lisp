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


;;; File: b/matlab:ode-translation
;;; Description: 


(in-package #I@FILE)

(include-declaration :expose-symbols (create-ode-model compute-indicies refresh-parameters)
                     :use-packages (mallavar-utility portable))                     

(include (@LIBRARY/matlab/settings
          @LIBRARY/math
          @LIBRARY/units/dimensionalization 
          @LIBRARY/math/ode-var))

(include @FOLDER/settings :expose)

(include-documentation 
  :description "Provides capability for translating little b temporal/reference/derived-vars to MatLab integration code."
  :organization "Harvard Medical School"
  :authors ("Aneil Mallavarapu"))
(define-function create-ode-model (name &key 
                                        vars
                                        (ode-comments t)
                                        (display-vars t)
                                        (overwrite :query)
                                        (integrator "ode15s")
                                        (abstol nil) 
                                        (reltol nil)
                                        (gauss-value :mean) ; may be mean or random
                                        (base-units (compute-default-base-units-from-vars vars)))

  (flet ((open-file (path)    
           (ensure-directories-exist path)
           (open path :if-exists :supersede :if-does-not-exist :create :direction :output))
         (failed-opening-file (path e)
           (format t "Failed to open ~A (~A)~%" path e)))
    (format t "~&Creating Matlab ODE ~A " name)
    (let+ ((*print-pretty*          nil)
           (vars                    (or vars (progn (format t "~&; Querying for variables...~%") (query-time-vars))))
           ((name path)             (parse-model-name name))
           (mm                      (make-matlab-model name
                                                       :path path
                                                       :ode-vars vars
                                                       :ode-comments-p ode-comments
                                                       :integrator integrator
                                                       :reltol reltol
                                                       :abstol abstol
                                                       :gauss-value gauss-value
                                                       :display-vars (order-vars (if (eq t display-vars) vars
                                                                                   display-vars))
                                                       :base-units  base-units))
           (run-path                (matlab-file-path (matlab-model-name mm) path))
           (init-path               (matlab-file-path (matlab-model-init-script-name mm) path))
           (indicies-path           (matlab-file-path (matlab-model-var-indicies-name mm) path)))
      (when (ok-to-overwrite overwrite run-path init-path indicies-path)
        (unless vars
          (warn "*** No ode vars selected ***~%"))
        (format t "~&; Computing Matlab code...~%")
        (let*protect
            ((run-file      (open-file run-path)  (close run-file))
             (init-file     (open-file init-path) (close init-file))
             (indicies-file (open-file indicies-path) (close indicies-file))
             (*math-print-function*   (compute-matlab-math-print-function mm)))
          (write-matlab-files run-file init-file indicies-file mm path)))
      (setf *model* mm))))

(define-function refresh-parameters (&optional (model *model*))
  "Rewrites the parameters/initial conditions of a model (default is *model*) to the model's initfile."  
  (let* ((name (matlab-model-init-script-name model))
         (path (matlab-model-path model))
         (init-path (matlab-file-path name path)))
    (with-open-file (file init-path :direction :output)
      (write-init-script file model))
    (format t "New parameters file (~A.~A) written to ~A."
            (pathname-name init-path)
            (pathname-type init-path)
            path)))

(defun make-safe-file-name (o)
  (substitute-if-not #\_ #'alphanumericp (string-downcase o)))

(defun matlab-file-path (name path)
  (if (streamp path) path  ; a little hack to allow redirection of output from files to a single stream
    (normalize-pathname (merge-pathnames (mkstr name ".m") path))))

(defun compute-indicies (varlist &optional (model *model*))
  (assert model (model)
    "No current model.  Please provide a model name.")
  (with-print-context t
    (format t "~&~A({'~A'~{,'~A'~}})"
            (matlab-model-var-indicies-name model)
            (first varlist) (rest varlist))))

(defun order-vars (vars)
  (format t "~&; Sorting variables")
  (mapcar #'cdr
          (sort (mapcar (lambda (v) (cons (format nil "~(~A~)" v) v)) vars) 
                #'string<
                :key #'car)))

(defun query-time-vars (&key prefer (base-set (query ode-var)))
  (compute-ode-vars base-set prefer))

(defun write-matlab-files (run-file init-file indicies-file mm path)
;  (handler-case 
      (progn 
        (write-run-file run-file mm)
        (write-var-indicies-file indicies-file mm)
        (write-init-script init-file mm)
        (format t "~&The following files have been written to ~A:~{~%     ~A.~A~}"
                (truename path)
                (mapcan (lambda (p) (list (carefully-get-filename p) (carefully-get-filetype p)))
                        (list run-file init-file indicies-file)))))
 ;   (error (e) (format t "Error while writing files: ~A" e))))


(defun write-global-declarations (file mm)
  (princ (global-declarations mm) file)
  (fresh-line file))

(defun global-declarations (mm)
  (format nil "global ~A ~A ~A;" 
          (matlab-model-descriptions-var-name mm)
          (matlab-model-initconds-var-name mm)
          (matlab-model-kvars-name mm)))

(defun write-init-script (file mm)
  (write-global-declarations file mm)
  (let ((iconds (with-output-to-string (stream) (write-initconds stream mm))))
    (write-kvars file mm)
    (princ iconds file))
  (write-descriptions file mm))

(defun write-run-file (file mm)
  (write-entry-point-function file mm)
  (write-calculate-odeset-function file mm)
  (write-ode-function file mm))


(defun write-var-indicies-file (file mm)
  (format file "function results = ~A(var_names)~%~
                  ~3T~A~%~
                  ~3T~A;~%~
                  ~3Tdescr_strings = cellstr(~A);~%~
                  ~3Tresults=[];~%~
                  ~3Tfor i = 1:length(var_names)~%~
                  ~6Tfor j = 1:length(descr_strings)~%~
                  ~9Tif findstr(var_names{i},descr_strings{j})~%~
                  ~12Tresults=horzcat(results,j);~%~
                  ~9Tend~%~
                  ~6Tend~%~
                  ~3Tend~%~%"
          (matlab-model-var-indicies-name mm)
          (global-declarations mm)
          (matlab-model-init-script-name mm)
          (matlab-model-descriptions-var-name mm)
          (matlab-model-descriptions-var-name mm)))


;;;
;;; helper functions which write matlab code to a stream:
;;;
(defun write-entry-point-function (file mm)
  (format file "function [t,y]=~A(timespan,varargin)~%~
                ~3T~A~%~
                ~3T~A(); % load initial conditions and parameters~%~%"
          (matlab-model-name mm)
          (global-declarations mm)
          (matlab-model-init-script-name mm))
  (with-print-context t
    (format file "~3T% timespan is in units of ~A.~%" *time-dimension*.(calculate-unit (matlab-model-base-units mm))))

  (format file "~3T[options,display]=calculate_odeset(varargin,nargin-1,cellstr(~A));~%"
          (matlab-model-descriptions-var-name mm))
  (format file "~3T[t,y]=~A(@~A,timespan,~A,options);~%"
          (matlab-model-integrator mm)
          (matlab-model-ode-function-name mm)
          (matlab-model-initconds-var-name mm))
  (format file "~3Tlegend(~A(display,:));~%~
                end~%~%" (matlab-model-descriptions-var-name mm)))

(defun write-calculate-odeset-function (file mm)
  (let* ((reltol (matlab-model-reltol mm))
         (abstol (matlab-model-abstol mm))
         (plotstr (if (matlab-model-display-vars mm) "'OutputFcn',@odeplot,'OutputSel',display" ""))
         (reltolstr (if reltol (format nil ",'RelTol',~A" reltol) ""))
         (abstolstr (if abstol (format nil ",'AbsTol',~A" abstol) "")))

    (format file "function [options,display]=calculate_odeset(vargs, nargs, descriptions)~%~
                  ~3Tif (nargs>=1)~%~
                  ~6Tdisplay=~A(vargs);~%~
                  ~3Telse ~%~
                  ~6Tdisplay=[1:length(descriptions)];~%~
                  ~3Tend~%~
                  ~3Toptions=odeset(~A~A~A);~%~
                  end~%~%" (matlab-model-var-indicies-name mm) plotstr reltolstr abstolstr )))

(defun write-ode-function (file mm)
  (format t "~&Writing ode functions")
  (let ((vars (matlab-model-ode-vars mm)))
    (format file "function dy = ~A(t,y)~%~
                  ~3T~A~%~
                  ~3Tdy = zeros(~S,1);~%"
            (matlab-model-ode-function-name mm)
            (global-declarations mm)
            (length vars))
    (loop for v in vars
          for i = 1 then (1+ i)
          for rate = (compute-ode-expression (nth (1- i) (matlab-model-ode-rates mm)) mm)
          do  (princ #\.)
              (with-print-context t 
                ;; write comment
                (if (matlab-model-ode-comments-p mm)
                    (b-format file "~3T% ~A = ~A ~%" v rate))
                (with-dimensionless-math (matlab-model-base-units mm)
                    (format file "~3Tdy(~S) = ~S;~%" i rate))))
    (format file "end~%~%")))

(defun compute-display-var-indicies (mm)
  (remove nil (mapcar (lambda (i) (ode-var-index i mm)) (matlab-model-display-vars mm))))


(defun write-initconds (file mm)
  (let ((vars (matlab-model-ode-vars mm)))
    (format file "~A = [ ..." (matlab-model-initconds-var-name mm))
    (format t "~&Writing initial conditions")
    (loop for v in vars
          for i = 1 then (1+ i)
          do (with-print-context t
               (let* ((base-units (matlab-model-base-units mm))
                      (the-unit v.dimension.(calculate-unit base-units)))
                 (with-dimensionless-math base-units
                   (b-format file "~%    ~A,... %" (print-math v.t0 nil)))
                 (b-format file " [~A] ~A in ~A" i v the-unit))))

    (format file "~%];~%~%")))

(defun escape-matlab-formatting-chars (str)
  (find-and-replace-all
   "^" "\\^"
   (find-and-replace-all 
    "_" "\\_" 
    (find-and-replace-all
     "\\" "\\\\"str))))
  
(defun write-descriptions (file mm)
  (flet ((write-var (v stream)
           (with-print-context t
             (let ((d v.dimension))
               (m-format stream
                         "\'~A~@[ (~S)~]'" 
                         v (unless (eq d null-dimension)
                             d.(calculate-unit (matlab-model-base-units mm))))))))
    (let ((vars (matlab-model-ode-vars mm)))
      (format file "~A=char( ...~%" (matlab-model-descriptions-var-name mm))
      (when vars
        (write-var (first vars) file)
        (loop for v in (rest vars)
              for i = 1 then (1+ i)
              do (format file ",... % [~S]~% " i)
              (write-var v file))
        (format file ");~%~%")))))


(defun m-format (stream str &rest args)
  "Ensures any matlab formatting chars are escaped"
  (princ (escape-matlab-formatting-chars (format nil "~?" str args)) stream))

(defun b-format (stream string &rest args)
  "Prints using b print style, creating strings suitable for matlab"
  (let ((*math-print-function* 'default-math-printer))
    (apply #'format stream string args)))

(defun write-kvars (file mm)
  (let* ((name (matlab-model-kvars-name mm))
         (kvars-table (matlab-model-kvars mm))
         (kvars  (make-array (1- (hash-table-count kvars-table)))))
    (loop for kvar being the hash-keys of kvars-table
          for i being the hash-values of kvars-table
          unless (eq kvar :last-index)
          do (setf (svref kvars (1- i)) kvar))
    (format file "~A= [...~%" name)
    (format t "~&Writing parameters")
    (with-print-context t
      (loop for k across kvars ;(matlab-model-kvars mm)
            for i = 1 then (1+ i)
            for k-dimension = k.dimension.(calculate-unit (matlab-model-base-units mm))
            for value = k.value
            do (princ #\.) ; dribble a dot to std-out
            if (math-expression-p value)
               do (with-dimensionless-math (matlab-model-base-units mm)
                    (format file "     0,... % "))
                  (b-format file "[~A] ~S = ~S (calculated below)~%" i k value)
               and collect (list i k) into dependent-vars
            else
               do  (with-dimensionless-math (matlab-model-base-units mm)
                     (format file "     ~a,... %" (print-math value nil)))
                   (b-format file " [~A] ~A (~A)~% "
                             i k k-dimension)
            finally 
            ;; close off the matrix
            (format file "];~%" file)
            ;; write any dependent vars:
            (loop for (i k) in dependent-vars
                  do (with-dimensionless-math (matlab-model-base-units mm)
                       (format file "~A(~A)=~S;~%" name i k.value)))
            (format file "~%")))))
  

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
;;;;   MATLAB-MODEL
;;;;
;;;;    structure which stores information about how MATLAB code is to be generated
;;;;

;;
;; the matlab-model structure stores information about how MATLAB code is to be generated
;;
(defstruct (matlab-model (:constructor _make-matlab-model 
                          (_name path ode-vars ode-rates base-units gauss-value kvars display-vars integrator abstol reltol ode-comments-p)))
  _name ; string indicating the name of the model
  path ; the path in which files for this model are stored
  ode-vars ; an ordered list of n ode-vars/derived-vars which map to y(1)...y(n) in the matlab ODE system
  ode-rates ; the .rates of the ode-vars
  base-units ; units (of base dimensions only) which all quantities are converted to
  gauss-value ; either :mean or :random - controls how gaussians are sampled G.mean or G.random-value
  kvars ; ordered list of vars which represent parameters (as opposed to ode-vars)
  display-vars ; a list of ode-vars/derived-vars which should be displayed in matlab
  integrator ; the matlab ode function to use (default = ode15s)
  abstol ; absolute tolerance for ode function
  reltol ; absolute tolerance for ode function
  ode-comments-p)  ; print comments for ode-models
 

(defmethod print-object ((mm matlab-model) stream)
  (print-unreadable-object (mm stream :type t :identity t)
    (format stream "~A (~A vars)" (matlab-model-name mm)
            (length (matlab-model-ode-vars mm)))))

(defun make-matlab-model (model-name 
                          &key 
                          (path *model-output-default-pathname*)
                          (ode-comments-p t)
                          (ode-vars (query [ode-var ?])) 
                          (base-units nil) 
                          (abstol nil)
                          (reltol nil)
                          (integrator "ode15s")
                          (gauss-value :mean) 
                          (display-vars nil))
  (let* ((rates (progn (format t "~&; Computing rates...") (loop for var in ode-vars
                                                                   do (princ #\.)
                                                                   collect var.rate)))
         (kvars (make-hash-table))) ; (order-vars (compute-kvars rates))))
    (setf (gethash :last-index kvars) 0)
    (_make-matlab-model model-name path ode-vars rates base-units gauss-value kvars display-vars integrator abstol reltol ode-comments-p)))

(defun matlab-model-name (mm)
  (etypecase mm
    (string mm)
    (matlab-model (matlab-model-_name mm))))

(defun matlab-model-init-script-name (mm)
  (mkstr (matlab-model-name mm) "_init"))
(defun matlab-model-kvars-name (mm)
  (mkstr (matlab-model-name mm) "_k"))
(defun matlab-model-descriptions-var-name (mm)
  (mkstr (matlab-model-name mm) "_descriptions"))
(defun matlab-model-initconds-var-name (mm)
  (mkstr (matlab-model-name mm) "_initial_conditions"))
(defun matlab-model-ode-function-name (mm)
  (mkstr (matlab-model-name mm) "_odefn"))
(defun matlab-model-var-indicies-name (mm)
  (mkstr (matlab-model-name mm) "_vars"))

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

(defun ode-var-index (v mm)
  (compute-var-index v (matlab-model-ode-vars mm)))

(defun kvar-index (v mm)
  (let ((kvars (matlab-model-kvars mm)))
    (or (gethash v kvars)
        (setf (gethash v kvars)
              (incf (gethash :last-index kvars))))))

;  (compute-var-index v (matlab-model-kvars mm)))


(defun compute-var-index (v vars)
  (let ((pos  (position v vars)))
    (if pos (1+ pos)
        (error "Unable to find ~S in variables ~S" v vars))))


(defgeneric compute-ode-expression (e mm)
  (:documentation "Given a var, derived-var or expression, returns an expression in terms of
the vars in (matlab-model-ode-vars mm).  Since DERIVED-VARs can be expressed in terms of reference-vars or
ODE-VARs, the system must decide which form to prefer.  By default normal-vars are prefered 
(reference-vars and ODE-VARs) - if a DERIVED-VAR is listed in (codefinfo-ode-vars mm), then it is prefered."))

(defmethod compute-ode-expression ((e reference-var) mm)
  (declare (ignorable mm)) e)
  
(defmethod compute-ode-expression ((e ode-var) mm)
  (or (find e (matlab-model-ode-vars mm))
      (let ((derived-var (find e (matlab-model-ode-vars mm) :key ?.normal-base)))
        (assert (derived-var-p derived-var) () "Cannot compute ode-expression of ~S" e)
        derived-var.normal-inverse-fn)))

(defmethod compute-ode-expression ((e derived-var) mm)
  (or (find e (matlab-model-ode-vars mm))
      (if (find e.normal-base (matlab-model-ode-vars mm))
          e.normal-fn)
      (assert nil () "Cannot compute ode-expression of ~S" e)))

(defmethod compute-ode-expression ((e math-expression) mm)
  e.(map (lambda (v) (compute-ode-expression v mm))))

(defmethod compute-ode-expression (o mm) 
  (declare (ignorable mm)) o)

(defun compute-matlab-math-print-function (mm)
  (let ((old-print-fn *math-print-function*))
    (labels ((matlab-math-print-function (object stream &optional (lhs-op nil))
               (typecase object
                 (var       (if (kvar-p object) (write-kvar-code object mm stream)
                              (write-ode-var-code object mm stream)))

                 (quantity  (with-dimensionless-math (matlab-model-base-units mm)
                              (matlab-math-print-function  (dimensionless object) stream lhs-op)))


                 (rational  (princ (coerce object 'float) stream))

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
                    (t     (prin1 object stream)))))))
             #'matlab-math-print-function)))

(defun write-ode-var-code (v mm stream)
  (format stream "y(~A)" ; note: y is a local variable in the matlab ode function
          (ode-var-index v mm)))

(defun write-kvar-code (v mm stream)
  (format stream "~A(~S)" 
          (matlab-model-kvars-name mm)
          (kvar-index v mm)))

;;;;
;;;; file helper functions
;;;;

(defun parse-model-name (p)
  "Breaks a pathname into 2 parts, the file name and directory, and checks that
   the name is valid for a matlab function."
  (let* ((p    (merge-pathnames p (or *model-output-default-pathname* *load-truename* "")))
         (name (pathname-name p))
         (path (make-pathname :name nil :type nil 
                              :defaults p)))
    (unless (eq (first (pathname-directory path)) :absolute)
      (warn "Output folder was not provided.  Files will be saved to ~A.~%~
             To avoid this warning, either provide an absolute pathname to CREATE-ODE-MODEL,~%~
             or set ~S to an absolute pathname in INIT.LISP.~%~
             E.g., something like this:
             ~8T(setf #/b/matlab/settings:*model-output-default-pathname* ~S)~%~%"  
            (truename path)
            '*model-output-default-pathname*
            #+:win32 #P"C:/Matlab7/work/"
            #+:unix #P"~/matlab/work/"))

    (check-matlab-function-name name)
    (values name path)))
                 
(defun check-matlab-function-name (str)
  (unless 
      (and (> (length str) 0)
           (valid-matlab-function-first-char-p (elt str 0))
           (or (< (length str) 2)
               (every #'valid-matlab-function-next-char-p
                      (subseq str 1))))
    (error "~S is not a valid MatLab function name." str)))

(defun valid-matlab-function-next-char-p (c)
  (or (alphanumericp c)
      (char= c #\_)))

(defun valid-matlab-function-first-char-p (c)
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
;;;;     (with-dimensionless-math (matlab-model-base-units ci)
;;;;       (let ((low-t0     (lowest-non-zero (matlab-model-ode-vars ci) ?.t0))
;;;;             (low-kvar   (lowest-non-zero (matlab-model-kvars ci) ?.value)))
;;;;         {low-t0 low-kvar}))))










