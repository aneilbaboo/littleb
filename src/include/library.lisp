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

;;; File: define
;;; Description: defines a global symbol, S, which is used as the parent value of
;;;              *ID* when generating new objects.  The object returned by the
;;;              form takes (id S) as its ID.

;;; $Id: library.lisp,v 1.1 2007/09/25 17:54:12 amallavarapu Exp $
;;;

;;;;
;;;; LIBRARIES:
;;;;
(in-package b)

(defparameter *library-search-paths* ()
  "An ordered list of pathnames to be searched when a library is required.
   This value may be set in the initialization file (use (get-b-path :init-file)
   to determine the location.")

(defparameter *library-info* (make-hash-table :test #'equalp)
  "A map from library names to library-info structures.")

;;;
;;; LIBRARY INFO
;;;
(defstruct library-info  
  pathname
;  ignore-paths
  documentation
  compile-directives)      ; list of include paths/:clear directives to be executed in order during 
                           ; (compile-library)

(defun clear-libraries ()
  (clrhash *library-info*))

(defun add-library (pathname &key (if-exists :error) (force-init nil))
  (let* ((pathname (normalize-pathname pathname))
         (name     (library-name pathname))
         (existing (ifit (gethash name *library-info*) 
                       (library-info-pathname it))))

    (when (and existing (not (pathname-match-p existing pathname))
               (eq if-exists :error))
      (with-simple-restart (continue "Replace with ~S" pathname)
        (error "Library ~A (~S) already exists." name
               existing)))

    (setf (library-info-pathname (library-info pathname)) pathname)

    (when (or (not existing)
              force-init)
      (load-library-init-file pathname))

    pathname))

(defun remove-library (pathname)
  (let ((name (library-name pathname)))
    (remhash name *library-info*)))

;;;; (defun library-compiled-filetype-p (filetype)
;;;;   (or (string-equal filetype +b-compiled-file-type+)
;;;;       (string-equal filetype +b-compiled-signature-file-type+)))

(defun ensure-library (libspec &optional force-init)
  "Ensures that the library indicated by the include path is registered and initialized"
  (let ((libname (include-path-library-name libspec)))
    (condit 
     ((find-existing-library libname) (if force-init (load-library-init-file it))
                                      it)

     ((find-library libname)          (add-library it :force-init force-init))

     (t                               (error "Cannot find library ~A." libname)))))


(defun find-library (spec)
  (let ((libname (library-name spec)))
    (or (find-existing-library libname)
        (loop with relpath = (make-pathname :directory `(:relative ,(string-downcase libname)))
              for libdir in *library-search-paths*
              for dir = (merge-pathnames relpath libdir)
              when #-clisp (probe-file dir)
                   #+clisp (and (directory dir) (pathname-directory-p dir))
              do (return dir)))))

(defun find-existing-library (spec)
  (whenit (gethash (library-name spec) *library-info*)
    (library-info-pathname it)))

(defun find-all-libraries ()
  (nconc
   (maphash-to-list (lambda (k v) 
                      (declare (ignorable k))
                      (library-info-pathname v)) *library-info*)
   (mapcan (lambda (spath)
             (loop for p in (directory spath)
                   when (and (pathname-directory-p p)
                             (not (find-existing-library p)))
                   collect p))
           *library-search-paths*)))
                  
(defun invalid-library-pathname-error (pathname)
  (let* ((pathdir (pathname-directory pathname))
         (len     (length pathdir)))
    (labels ((compute-restarts (n)
               (cond
                ((= n len) (error "Cannot find library for ~A." pathname))
                (t
                 (let ((libpath    (make-pathname :host (pathname-host pathname)
                                                  :device (pathname-device pathname)
                                                  :directory (subseq pathdir 0 (1+ n))))
                       (name       (string-upcase (nth n pathdir))))
                   (restart-case (compute-restarts (1+ n))
                     (continue ()
                               :report (lambda (stream) 
                                         (format stream "Add new library named ~A (~S)." name libpath))
                               (add-library libpath))))))))
      (compute-restarts 1))))

(defun library-name (x)
  "Returns the name (an uppercase string) for the library indicated by spec, which
may be a string, symbol, pathname or library-info structure"
  (etypecase x
    (keyword     (library-name (get-b-path x)))
;    (cons        (get-b-path (first x) (rest x)))
    (string      (include-path-library-name x))
    (symbol      (library-name (symbol-name x)))
    (include-path (include-path-library-name x))
    (pathname    (assert (pathname-directory-p x) (x))
                 (library-name (first (last (pathname-directory x)))))
    (library-info (library-name (library-info-pathname x)))))


(defun library-info (spec)
  "Finds or creates a library-info structure for the library name specified by spec."
  (let* ((libname (library-name spec))
         (current (gethash libname *library-info*)))
    (or current
        (setf (gethash libname *library-info*) (make-library-info)))))

;;;; (defun library-ignore-paths (&optional (lib (compute-current-library)))
;;;;   (library-info-ignore-paths (library-info lib)))

;;;; (defun (setf library-ignore-paths) (paths &optional (lib (compute-current-library)))
;;;;   (setf (library-info-ignore-paths (library-info lib)) (ensure-list paths)))


(defun compute-current-library ()
  (multiple-value-bind  (ipath lib)
      (compute-current-include-path)
    (declare (ignorable ipath))
    lib))

(defun library-compile-directives (&optional (lib (compute-current-library)))
  (library-info-compile-directives (library-info lib)))

(defun (setf library-compile-directives) (directives &optional (lib (compute-current-library)))
  (setf (library-info-compile-directives (library-info lib)) directives))

(defun library-documentation (&optional (lib (compute-current-library)))
  (library-info-documentation (library-info lib)))

(defun (setf library-documentation) (doc-str &optional (lib (compute-current-library)))
  (setf (library-info-documentation (library-info lib)) doc-str))

;;;
;;; IGNORE-PATHS - a system for keeping track of which paths to ignore when in a particular path
;;;

(defvar *ignore-paths* (mtrie:make-trie (lambda (x y) (pathname-descendent-p x y t)) #'pathname-match-p))

(defun current-directory ()
  (let ((dir (or *load-truename* *compile-file-truename*)))
    (if dir (if (pathname-directory-p dir) dir (pathname-parent dir)))))

(defun ignore-paths (&optional (dir (current-directory)  dirp))
  (let+ ((dir              (if dir (pathname dir)))
         ((node ancestors) (mtrie:find-trie-node dir *ignore-paths*))
         (all              (nconc (if node (list node))                                 
                                  ancestors)))
    (apply #'nconc (delete-if #'null (map-into all #'mtrie:trie-node-value all)))))

(defun (setf ignore-paths) (value &optional (dir (current-directory) dirp))
  (let* ((dir   (if dir (pathname dir)))
         (*default-pathname-defaults* (or dir #P"/")))
    (setf (mtrie:trie-node-value (mtrie:find-trie-node dir *ignore-paths* t))
          (mapcar #'merge-pathnames (ensure-list value)))))

(defun ignore-path-p (path &optional (dir (current-directory)))
  (let ((path (if path (pathname path)))
        (dir  (if dir (pathname dir))))
    (if (find-if (lambda (ignore-path)
                   (when ignore-path (pathname-match-p path (merge-pathnames ignore-path dir))))
                 (apply #'append 
                        (mapcar #'mtrie:trie-node-value 
                                (mtrie:trie-ancestors dir *ignore-paths*))))
        t)))

;;
;;; LIBRARY INIT FILE:
;;;

(defun library-init-file (library-pathname)
  (probe-file
   (merge-pathnames (make-pathname :name "_config" :type +b-source-file-type+)
                    library-pathname)))

(defun load-library-init-file (pathname)
  "Loads the library init file."
  (let* ((pkg                        (make-b-package (gensym "_CONFIG")))
         (init-file                  (library-init-file pathname)))
    (assert (eq :absolute (first (pathname-directory pathname))) (pathname)
      "Invalid pathname passed to LOAD-LIBRARY-INIT-FILE: ~A." pathname)
    (unwind-protect
        (when init-file
          (with-edit-and-retry-restart (init-file)
            (let ((*package* pkg)
                  (*readtable* +b-readtable+))
              (load init-file))))
      (delete-package pkg))))

(defun reload-include-path (include-path)
  (declare (special *current-loaded-ipaths*))
  (let ((ipath (include-path include-path)))
    (setf (include-path-current-signature ipath) nil
          (currently-loaded-p ipath) nil)
    (include-dynamic ipath nil :force t)))

(defun reload-include-paths (ipaths)
  (dolist (ipath ipaths) (reload-include-path ipath)))


;;;
;;; helper functions
;;;

(defun libraries-config-file (dir)
  (merge-pathnames "libraries.config" dir))

(defun read-libraries-config (dir)
  "Returns IGNORE-DIRS, ORDER"
  (let ((pkg         (make-package (gensym) :use ()))
        (config-file (libraries-config-file dir))
        (ignore      ())
        (order       ()))
    (when (probe-file config-file)
      (labels ((resolve-sub-dir (s) 
                 (merge-pathnames (make-pathname :directory `(:relative ,s)) dir))
               (resolve-args (args) (mapcar #'resolve-sub-dir args))
               (order (&rest args) (setf order (nconc (resolve-args args) order)))
               (ignore (&rest args) (setf ignore (nconc ignore (resolve-args args)))))
        (setf (symbol-function (intern "IGNORE" pkg)) #'ignore
              (symbol-function (intern "ORDER" pkg)) #'order)      
        (handler-case (unwind-protect (let ((*package* pkg)) (load config-file))
                        (delete-package pkg))
          (error (e) (error "~A while initializing libraries folder: ~A" e dir)))
        (values ignore order)))))
            
(defun library-bin-dir (lib)
  "Where to put compiled dirs for each implementation."
  (normalize-pathname (make-pathname :defaults lib
                                     :directory (append (pathname-directory lib)
                                                        (list "_bin")))))


(defun library-compiled-dir (lib)
  "The path for the compiled files for the currently running implementation"
  (normalize-pathname (make-pathname :defaults lib
                                     :directory (append (pathname-directory (library-bin-dir lib))
                                                        (list *b-compile-name*)))))

(defun library-compiled-signature-mismatches (lib)
  "Returns include-paths for which the compiled signature does not match the source signature."
  (loop for path in (include-path-descendents lib)
        for file = (probe-file (include-path-source-file path))
        for (src-sig cpl-sig) = (if file (list (include-path-source-signature file)
                                               (include-path-compiled-signature path)))
        when (not (equalp src-sig cpl-sig))
        collect path))

(defun make-safe-dir-name (&rest strs)
  (substitute-if-not #\- #'alphanumericp (format nil "~{~A~^-~}" strs)))


(defun delete-littleb-binaries (&key (system t) (libraries t))
  (when libraries (dolist (lib (find-all-libraries)) ; *libraries*)
                    (delete-directory (library-bin-dir lib) t)))
  (when system
    (delete-directory (symbol-value (find-symbol "*B-ALL-VERSIONS-COMPILED-PATH*" 'cl-user)) t)))

; +------------------------------------------
;    User interface helper functions
;    Need to provide non-capi implementations of
;    prompt-for-confirmation (y-or-n-p)
;    prompt-with-list (N/A)
;    prompt-for-string (N/A)
; +------------------------------------------
#+capi
(eval-when (:compile-toplevel :load-toplevel :Execute)
;;;; (defvar *current-library* nil)
;;;; (defun prompt-for-library (&key (prompt "Select library") 
;;;;                                 (selection *current-library*))
;;;;   *current-library*)
;;;;   (let* ((position  (or (if (pathnamep selection) 
;;;;                             (position selection libraries :test #'mallavar-utility:pathname-equal)
;;;;                           selection)
;;;;                         (1- (length libraries))))
;;;;          (result   (capi:prompt-with-list libraries prompt :selection position)))
;;;;     (when result (setf *current-library* result))
;;;;     result))


;;;; (defun prompt-for-new-include-file (&optional (default (include-path *package*)))
;;;;   "Returns 3 values: INCLUDE-PATH, LIBRARY, PATHNAME or NIL,NIL,NIL"
;;;;   (loop for name = (capi:prompt-for-string (format nil "Enter name for new package (\"<\" refers to ~A):" default)
;;;;                                            :accept-null-string nil)
;;;;         for ipath = (when name (ignore-errors (include-path name default)))
;;;;         for (existing lib) = (when ipath (multiple-value-list (include-path-source-file ipath)))
;;;;         unless ipath
;;;;         do (return (values nil nil nil))
;;;;         if existing
;;;;         do (if (capi:prompt-for-confirmation (format nil "~A already exists in library ~A. ~%~%Use this file instead?"
;;;;                                                      ipath lib))
;;;;                (return (values ipath lib existing)))
;;;;         else
;;;;         do (let* ((lib (prompt-for-library :prompt (format nil "Choose library for ~A" ipath)))
;;;;                   (new (when lib (include-path-source-file ipath))))
;;;;              (when new
;;;;                (write-include-file-boilerplate new)
;;;;                (cond
;;;;                 (lib  (return (values ipath lib new))
;;;;                       (return (values nil nil nil))))))))
;;;;   
;;;; (defun write-include-file-boilerplate (file &key 
;;;;                                             (ipath "B-USER")
;;;;                                             (description "No documentation yet.")
;;;;                                             (authors '("Unknown author"))
;;;;                                             (organization "Unknown organization"))
;;;;   (when (probe-file file)
;;;;     (b-error "Attempt to overwrite ~A." file))
;;;;   (ensure-directories-exist file)
;;;;   (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :error)
;;;;     (format stream "(in-package ~S) ; alternatively, #I@FILE, #I@FOLDER, #I@LIBRARY~%~%~
;;;;                     (include-documentation :description ~S ~%~
;;;;                                        ~23T:authors ~S~%~
;;;;                                        ~23T:organization ~S)~%~%" 
;;;;             ipath
;;;;             description authors organization)))
)


;;;; (defun add-all-libraries-in-directory (dir)
;;;;   (let+ (((ignore order) (read-libraries-config dir))
;;;;          (dirs           (remove-if (lambda (o) (or (member o ignore :test #'pathname-equal)
;;;;                                                     (not (pathname-directory-p o))))
;;;;                                     (delete-duplicates (append order (directory dir))
;;;;                                                        :test #'pathname-equal
;;;;                                                        :from-end t))))
;;;;     (dolist (path dirs)
;;;;       (add-library path))))

;;;; (defun *libraries*-with-new-pathname (pathname position)
;;;;   "Returns *libraries* with PATHNAME at some position in the search path,
;;;;    Where position is either a positive integer (pathname searched nth),  
;;;;                             :first (pathname searched first),
;;;;                             :LAST (pathname searched last), or
;;;;                             an existing library name (searched just before that library)."
;;;;   (let* ((pathname    (pathname pathname)))
;;;;     (etypecase position
;;;;       ((eql :new-last) (if (find pathname *libraries* :test #'pathname-equal) *libraries*
;;;;                          (*libraries*-with-new-pathname pathname :last)))       
;;;;       ((eql :last)     (*libraries*-with-new-pathname pathname (length *libraries*))) ;; new pathname searched last
;;;;       ((eql :first)    (*libraries*-with-new-pathname pathname 0)) ;; new pathname searched first
;;;;       ((eql :ignore-others) (list pathname))
;;;;       (pathname        (let ((pos (position position *libraries* :test #'pathname-equal)))
;;;;                          (unless pos (error "Cannot find ~S in ~S." position '*libraries*))
;;;;                          (*libraries*-with-new-pathname pathname pos)))
;;;;       (integer         (load-library-init-file pathname)
;;;;                        (delete-duplicates (insert pathname position *libraries*) :test #'pathname-equal)))))


;;;; (defun add-library-ignore-paths (lib paths)
;;;;   (let ((paths (ensure-list paths))
;;;;         (info  (library-info lib)))
;;;;     (setf (library-info-ignore-paths info)
;;;;           (union (library-info-ignore-paths info)
;;;;                  (mapcar (lambda (p)
;;;;                            (merge-pathnames p lib))
;;;;                          paths)
;;;;                  :test #'equalp))))

;;;; (defun set-library-compile-directives (lib directives)
;;;;   (let ((info       (library-info lib))
;;;;         (dprocessed (mapcar (lambda (d)
;;;;                               (if (consp d) (cons (first d) (mapcar #'include-path (rest d)))
;;;;                                 (include-path d)))
;;;;                             directives)))
;;;;     (setf (library-info-compile-directives info) dprocessed)))



;;;; (defun load-library-init-file (pathname)
;;;;   "Loads the library init file."
;;;;   (let* ((pkg                        (make-package (gensym "_config") :use ()))
;;;;          (reload-sym                 (intern "RELOAD" pkg))
;;;; ;         (libname                    (library-name pathname))
;;;;          (*library-pathname*         (pathname pathname))
;;;;          (init-file                  (library-init-file *library-pathname*)))
;;;;          
;;;;     (declare (special *library-pathname*))
;;;;     (assert (eq :absolute (first (pathname-directory pathname))))
;;;;     (unwind-protect
;;;;         (when init-file
;;;;           (with-edit-and-retry-restart (init-file)
;;;;             (import `(ignore-paths
;;;;                       when-compiling
;;;;                       clear
;;;;                       library-documentation
;;;;                       ,reload-sym) pkg)
;;;;             (setf (symbol-function reload-sym) #'reload-include-paths)
;;;;             (let ((*package* pkg))
;;;;               (load init-file :package pkg))))
;;;;       (delete-package pkg))))


;;;; (defun library-create-include-path (ipath &key create-file 
;;;;                                         (description "No documentation yet.") 
;;;;                                         (authors '("Unknown Author"))
;;;;                                         (organization "Unknown Organization"))

;;;;   (let ((file (include-path-source-file ipath)))
;;;;     (when (and create-file (not (probe-file file)))
;;;;       (write-include-file-boilerplate file ipath :description description :authors authors :organization organization))
;;;;     file))

