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

;;; File: include-path
;;;
;;; Description: Include paths are of the form: ICOMPONENT{\COMPONENT}*
;;;    Where ICOMPONENT and COMPONENT are non-whitespace-containing strings of characters.
;;;          ICOMPONENT may be a library name or one of 
;;;                   @LIBRARY - refers to the current library
;;;                   @FILE    - refers to the include path of the current file
;;;                   @PACKAGE or @ - refer to the include path represented by the 
;;;                                   current package name
;;;
;;;    For example, (INCLUDE B/BIOCHEM/SPECIES :USE) ensures that the file species.lisp
;;;        is loaded (from the biochem folder in the b library), and causes
;;;        that uses symbols from that file in the current package.
;;;
;;;        A file in the B library, could use the following directive instead:
;;;                 (INCLUDE @LIBRARY/BIOCHEM/SPECIES :USE)
;;;
;;;        A file in the same folder as species.lisp could use:
;;;                 (INCLUDE @FOLDER/SPECIES :USE)
;;;
;;;
;;;    Traversal components:  {<|>}+
;;;    The package components consisting of one or more < (>) characters are used to traverse
;;;        upward (downward) from the current path.  The number of transitions is 1 - the number of 
;;;        < (>) characters.
;;;
;;;    If a traversal component is first, the current path is given by the package context.
;;;
;;;    For example, in package A/B/C/D/E:
;;;    
;;;    @</X     ==>  A/B/C/D/E/X
;;;    @<</X    ==>  A/B/C/D/X
;;;    @<<</X   ==>  A/B/C/X
;;;
;;;    @>/X     ==>  A/X
;;;    @>>/X    ==>  A/B/X
;;;    @>>>/X   ==>  A/B/C/X
;;;
;;;    The #I reader macro can be used to generate strings based on include-path specifiers
;;;        These are helpful for generating package names automatically:
;;;
;;;        e.g.,  inside the file C:/code/b1/libraries/mylib/a/b/c.lisp,
;;;         (in-package #I@file) =R=> (IN-PACKAGE "MYLIB/A/B/C")
;;;         (in-package #I@library) =R=> (IN-PACKAGE "MYLIB")
;;;         (in-package #I@library/x/y) =R=> (IN-PACKAGE "MYLIB/X/Y")
;;;         (in-package #I@file/<<) =R=> (IN-PACKAGE "MYLIB/A/B")
;;;
;;;    Numeric arguments to the #I macro reader produce a variety of objects related to
;;;         the include 
;;;    #0spec => include-path object
;;;    #1spec => package
;;;    #3spec => source file pathname
;;;    #4spec => compiled file pathname
;;;
;;; $Id: include-path.lisp,v 1.6 2008/08/21 14:57:34 amallavarapu Exp $
;;;




(in-package b)

(defconstant +b-source-file-type+ "lisp") ; (car portable:*file-extensions*))
(defconstant +b-compiled-file-type+ (cdr portable:*file-extensions*))
(defconstant +b-signature-file-type+ "sig")
;(defconstant +include-library-include-path-components+ '("_library"))

(defvar *include-path-package-name-cache* (make-hash-table :test #'equalp)
  "Keeps a record of include path package names - used by include-path-package")
(defvar *compile-package* ())
(defvar *include-suppress* nil
  "Prevents the include system from operating - like READ-SUPPRESS")

(declaim (special *library-search-paths* *library-info*))
;;; HELPERMACROS & STUFF
(eval-when (:load-toplevel :compile-toplevel :execute)
(defmacro with-dummy-b-package (&body body)
  (let ((dummy-pkg '#:dummy-pkg))
    `(let ((,dummy-pkg      (make-package (mkstr "TMP-PKG" (incf *gensym-counter*)) :use '(cl b))))
       (unwind-protect 
           (let ((*package*     ,dummy-pkg))
             ,@body)
         (delete-package ,dummy-pkg))))))

(defun compute-current-include-path (&optional (errorp t))
  (cond
   (*compile-file-truename* (include-path-from-pathname *compile-file-truename* :errorp errorp))
   (*load-truename*         (include-path-from-pathname *load-truename* :errorp errorp))
   (errorp                  (error "COMPUTE-CURRENT-INCLUDE-PATH called in Listener."))))


;;;;
;;;; INCLUDE-PATH STRUCTURE & ACCESSORS
;;;;
(defstruct (include-path (:constructor make-include-path (_library-name _components _type)))
  (_library-name () :read-only t)
  (_components () :read-only t)
  (_type () :read-only t))

(defun include-path-library-name (include-path)
  (whenit (include-path include-path)
    (include-path-_library-name it)))

(defun include-path-components (include-path)
  (whenit (include-path include-path)
    (include-path-_components it)))

(defun include-path-type (include-path)
  (whenit (include-path include-path)
    (include-path-_type it)))

(defun include-path-parent (include-path)
  (include-path (butlast (include-path-to-list include-path))))

(defun include-path-ancestors (include-path)
  (let ((ipath (include-path include-path)))
    (whenit (include-path-parent ipath)
      (cons it (include-path-ancestors it)))))

(defun include-path-ancestor-p (include-path maybe-ancestor)
  (let* ((ipath               (include-path include-path))
         (list          (include-path-to-list ipath))
         (len                 (length list))
         (aipath              (include-path maybe-ancestor))
         (alist         (include-path-to-list aipath))
         (alen                (length alist)))
    (and (< alen len)
         (equalp alist
                 (subseq list 0 alen)))))

(defun include-path-children (&optional include-path)
  (delete-duplicates (mapcar #'include-path-from-pathname (include-path-subpathnames include-path)) :test #'equalp))

(defun include-path-descendents (&optional include-path)
  (labels ((descend (path)
             (let ((children (include-path-children path)))
               (nconc children
                      (loop for child in children
                            nconc (descend child))))))
    (delete-duplicates (descend include-path) :test #'equalp)))

;;;
;;; PACKAGE HANDLING
;;;
(defun include-path-package-name (include-path &optional (errorp t))
  "Determines the package from the source file, from a cache or by reading the file."
  (handler-case 
      (let* ((*include-suppress*  nil)
             (ipath          (include-path include-path))
             (pkg-sig        (gethash ipath *include-path-package-name-cache*))
             (pkg-name       (car pkg-sig))
             (sig            (cdr pkg-sig))
             (cur-sig        (if sig (include-path-current-signature ipath) '#:MUST-READ)))
        (declare (ignorable *include-suppress*))
        (cond
         ((equalp sig cur-sig)    pkg-name)
         (t
          (let ((pkg-name (read-in-package-name-from-file (include-path-source-file ipath))))
            (setf (gethash ipath *include-path-package-name-cache*) 
                  (cons pkg-name sig))
            pkg-name))))
    (error (e) (if errorp (error e)))))

(defun include-path-package (include-path &optional (create :error))
  "Input: an include path and an optional param,
CREATE = (NIL - do not create, T - force create, :ERROR - error if pkg doesn't exist
Returns 2 values - the package (or nil), created (bool representing whether it was created"
  (let+ ((ipath          (include-path include-path))
         (pkg            (find-or-create-b-package (include-path-package-name ipath) create)))
    (package-mark-item-clearable pkg ipath)
    pkg))

(defun find-or-create-b-package (pkg-name &optional (create :error))
  (condit
   ((find-package pkg-name) (use-package '(b cl) (package-mark-item-clearable it))
    (values it nil))
   ((eq create t)           (values (package-mark-item-clearable (make-b-package pkg-name))
                                    t))
   (t                       (if (eq create :error)
                                (error "Package ~A does not exist." pkg-name))
                            (values nil nil))))

(defun make-b-package (name)
  (make-package name :use '(b cl)))


(defmacro with-temporary-b-package ((&optional (name (mkstr "TMP-B-PKG" (incf *gensym-counter*))))
                                    &body body)
  (let ((tmp-pkg (gensym "PKG")))
    `(let ((,tmp-pkg (make-b-package ,name)))
       (unwind-protect 
           (let ((*package* ,tmp-pkg)) ,@body)         
         (delete-package ,tmp-pkg)))))

(defun read-in-package-name-from-file (file)
  "Returns a string representing the correct (resolved) package name for file."
  (let ((*load-pathname* file)
        (*load-truename* file)
        (*compile-file-pathname* file)
        (*compile-file-truename* file))
    (with-open-file-for-edit-and-retry (stream file :direction :input)
      (let* ((form (with-dummy-b-package (read stream nil))))
        (cond 
         ((in-package-form-p form)
          (return-from read-in-package-name-from-file (include-path-spec (second form))))
         (t
          (error "Missing (IN-PACKAGE ...) at start of ~A. ~@[\"~A\" is invalid.~]" file form)))))))

(defun find-ipath-package (ipath)
  (let ((name (include-path-spec ipath)))
    (find-package name)))
        
(defun include-path-spec (spec)
  (let ((ipath (include-path spec)))
    (format nil "~A~@[.~A~]"
            (apply #'delim-mkstr #\/ (cons (or (include-path-library-name ipath) "")
                                           (include-path-components ipath)))
            (include-path-type spec))))

       
;;;
;;; INCLUDE-PATH PATHNAMES:  CL PATHNAME objects representing the underlying files and directories
;;;
(defun include-path-subpathnames (&optional include-path)
  (let* ((ipath        (include-path include-path))
         (libname      (include-path-library-name ipath))
         (library      (add-library (find-library libname)))
         (subpaths     (directory (merge-pathnames "*.*" (include-path-source-dir ipath))))
         (bin-dir      (library-bin-dir library))
         (result       (remove-if (lambda (subpath)
                                    (or (pathname-match-p subpath bin-dir)
                                        (or (and (not (string-equal (pathname-type subpath)
                                                                    +b-source-file-type+))
                                                 (not (pathname-directory-p subpath)))
                                            (equalp subpath (library-init-file library)))
                                        (ignore-path-p subpath library)))
                                  subpaths)))
    
    (map-into result #'normalize-pathname result)))

(defun new-library-from-pathname (pathname preserve-type)
  (labels ((libpath-from-spath (spath)
             (merge-pathnames (make-pathname :directory `(:relative ,(nth (length (pathname-directory spath))
                                                                          (pathname-directory pathname))))
                              spath
                              nil)))

    ;; look for a match in search paths
    (loop with p-dir-len = (length (pathname-directory pathname))
          with filtered-search-paths = (remove-if (lambda (spath) 
                                                    (<= p-dir-len
                                                        (length (pathname-directory spath))))
                                                  *library-search-paths*)
          for libspath in filtered-search-paths
          for src-libpath = (merge-pathnames #P"*/" libspath nil)
          for cpl-libpath = (library-compiled-dir src-libpath)
          for src-wildpath = (merge-pathnames #P"**/*.*" src-libpath nil)
          for cpl-wildpath = (merge-pathnames #P"**/*.*" cpl-libpath nil)
          when (pathname-match-p pathname cpl-wildpath)
          do (return (__ipath-from-pathname-return-values pathname (libpath-from-spath libspath) cpl-libpath preserve-type))
          when (pathname-match-p pathname src-wildpath)
          do (return (__ipath-from-pathname-return-values pathname (libpath-from-spath libspath) src-libpath preserve-type)))))

(defun existing-library-from-pathname (pathname preserve-type)
  ;; search existing library info structs
  (loop with relwild = #.(make-pathname :name :wild :type :wild :version :wild :directory '(:relative :wild-inferiors))
        for libinfo being the hash-values of *library-info*
        for libpath = (library-info-pathname libinfo)
        for lib-cpath = (library-compiled-dir libpath)
        for wildpath = (merge-pathnames relwild libpath)
        for c-wildpath = (merge-pathnames relwild lib-cpath)
        when (pathname-match-p pathname c-wildpath)
        do (return (__ipath-from-pathname-return-values pathname libpath lib-cpath preserve-type))
        when (pathname-match-p pathname wildpath)
        do (return (__ipath-from-pathname-return-values pathname libpath libpath preserve-type))))

(defun __ipath-from-pathname-return-values (pathname lib libpath preserve-type)
  (let* ((pdir        (pathname-directory pathname))
         (lib-name    (nth (1- (length (pathname-directory lib))) pdir))
         (cpts        (append 
                       (nthcdr (length (pathname-directory libpath)) pdir)
                       (list (format nil "~A~@[.~A~]" (pathname-name pathname) 
                                     (when preserve-type
                                       (pathname-type pathname)))))))
    (list (include-path (cons lib-name cpts))
          (add-library lib))))
           
(defun include-path-from-pathname (pathname &key (errorp t) (preserve-type nil))
    (unless (or (null errorp)
                (stringp pathname)
                (pathnamep pathname))
      (invalid-library-pathname-error pathname))
    (when pathname
      (apply #'values
             (or (existing-library-from-pathname pathname preserve-type)
                 (new-library-from-pathname pathname preserve-type)
                 (if errorp (invalid-library-pathname-error pathname))))))



;;; 
;;; SOURCE FILE INFO:
;;; 
(defun include-path-source-file (include-path)
  "Returns: PATH of the file, Library which contains the file."
 (make-include-pathname include-path +b-source-file-type+))

(defun include-path-source-file-exists-p (path) 
  (probe-file (include-path-source-file path)))

(defun include-path-source-dir (spec)
  (let ((ipath (include-path spec)))
    (merge-pathnames (make-pathname :directory `(:relative ,@(mapcar #'string-downcase
                                                                     (include-path-components ipath))))
                     (ensure-library ipath))))

(defun include-path-source-dir-existsp (path)
  (probe-file (include-path-source-dir path)))

;;;
;;; SOURCE SIGNATURE:
;;;
;;;; (defun include-file-signature (pathname)
;;;;   (whenit (and pathname (probe-file pathname))
;;;;     (file-write-date it))) ; currently, signature is defined as the file-write-date
;;;;                            ; but, in future, this might be an MD5 sum or some other value.


(def-binding-environment *dependency-cache* (make-hash-table :test #'equalp))
(def-binding-environment *signature-cache* (make-hash-table :test #'equalp))

(defun include-path-source-signature (ipath)
  (with-binding-environment (*signature-cache*)
    (let* ((ipath      (include-path ipath)))
      (condit
       ((gethash ipath *signature-cache*) it)
       (t
        (let* ((sigfile    (include-path-source-signature-file ipath))
               (true-sigfile (probe-file sigfile))
               (stored-sig (when true-sigfile 
                             (ignore-errors (with-open-file 
                                                (stream true-sigfile :direction :input)
                                              (read stream)))))
               (sig-time   (cdr (first stored-sig)))
               (src-file   (probe-file (include-path-source-file ipath)))
               (true-sig   (cond
                            ;; signature file is up to date:
                            ((and sig-time src-file
                                  (equalp sig-time (file-write-date src-file)))      
                             stored-sig)
                            
                            ;; must recompute & store signature:
                            (src-file (let ((new-sig (compute-include-path-signature ipath)))
                                        (with-open-file (stream sigfile :direction :output :if-exists :supersede)
                                          (prin1 new-sig stream))
                                        new-sig)))))
          (setf (gethash ipath *signature-cache*) true-sig)))))))
   
(defun clear-include-path-source-signature (ipath)
  (setf (gethash ipath *signature-cache*) nil)
  (whenit (probe-file (include-path-source-signature-file ipath))
    (delete-file it)))

(defun signature-write-time (sig)
  (cdr (first sig)))

(defun include-path-source-signature-file (ipath)
  (make-include-pathname ipath +b-signature-file-type+))

(defvar *include-signature* (make-hash-table :test #'equalp)
  "A map from INCLUDE-PATH objects to the signature of the currently loaded file.")

(defun include-path-current-signature (ipath)
  "Returns the 'signature' of the currently loaded file or nil"
  (gethash (include-path ipath) *include-signature*))

(defun (setf include-path-current-signature) (sig ipath)
  (let ((ipath (include-path ipath)))
    (if sig
        (setf (gethash ipath *include-signature*) sig)
      (remhash ipath *include-signature*))))

(defun clear-signatures ()
  (clrhash *include-signature*))

(defun include-path-direct-dependencies (ipath)
  (let* ((ipath (include-path ipath))
         (pathname (include-path-source-file ipath))
         (*compile-package* (include-path-package-name ipath))
         (*include-suppress* t))
    (with-temporary-b-package ()
      (with-open-file (stream pathname :direction :input)
        (with-load-path pathname
          (remove-duplicates
           (mapcar #'include-path 
                   (loop with eof = '#:eof
                         for form = (read stream nil eof)
                         while (include-header-form-p form)
                         when (include-form-p form)
                         append (ensure-list (second form))))
           :test #'equalp))))))

(defun include-path-dependencies (ipath)
  "Transitive dependency list"
  (let ((ipath (include-path ipath)))
    (or (and *signature-cache* 
             (mapcar (lambda (x) (include-path (car x)))
                     (rest (gethash ipath *signature-cache*))))
;        (progn (format t "NO DEPENDENCIES FROM SIG CACH ~S~&" ipath)
 ;         nil)
        (with-binding-environment (*dependency-cache*)
          (let+ (((deps cached) (gethash ipath *dependency-cache*)))
            (cond 
             (cached deps)
             (t ;(format t "COMPUTING DEPENDENCIES FOR ~S~&" ipath)
                (let ((direct-deps (include-path-direct-dependencies ipath)))
                  (setf (gethash ipath *dependency-cache*)
                                 (remove-duplicates 
                                  (apply #'append
                                         direct-deps
                                         (mapcar (lambda (d)
                                                   (include-path-dependencies d)) 
                                                 direct-deps))))))))))))
               
(defun compute-include-path-signature (ipath)
  (let ((ipath (include-path ipath)))
    (mapcar (lambda (i)
              (cons (include-path-spec i) (file-write-date (include-path-source-file i))))
            (remove-duplicates (cons ipath (include-path-dependencies ipath))
                               :test #'equalp))))

                     
;;;
;;; COMPILED-FILE:
;;;
(defun include-path-compiled-file (spec)
  (make-include-pathname spec +b-compiled-file-type+ :bin))
                   
;;;
;;; COMPILED-SIGNATURE:
;;; 
(defun include-path-compiled-signature-file (include-path)
  (make-include-pathname include-path 
                         +b-signature-file-type+ 
                         :bin))

(defun include-path-compiled-signature (include-path)
  "The signature of the source file from which the compiled file was generated."
  (let+ (((path lib) (include-path-compiled-signature-file include-path)))
    (with-open-file (stream path :if-does-not-exist nil)
      (when stream
        (let ((signature (read stream nil)))
          (unless (consp signature)
            (warn "Invalid signature file ~S - deleting compiled file." path)
            (close stream)
            (delete-file path)
            (whenit (probe-file (include-path-compiled-file include-path))
              (delete-file it)))
          (values signature lib))))))

(defun (setf include-path-compiled-signature) (signature 
                                               spec)
  (let ((path (include-path-compiled-signature-file spec)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (prin1 signature stream))))

;;;;
;;;; PATHNAMES GENERATED FROM AN INCLUDE-PATH:
;;;;
(defun include-path-exists-p (spec)
  (probe-file (include-path-source-file spec)))

(defun include-path-effective-file (spec)
  "Returns NIL or 4 values: the file which should be loaded, its signature, the library, and either :binary or :SOURCE"
  (let+ ((ipath     (include-path spec))
         ((src lib) (include-path-source-file ipath))
         (src-sig   (include-path-source-signature ipath))
         (cpl       (probe-file (include-path-compiled-file ipath)))
         (cpl-sig   (include-path-compiled-signature ipath)))
        (cond
         ((and cpl
               (or (null src-sig) ; there's no source file
                   (equalp cpl-sig src-sig))) ; or the compiled file is up to date
          (values cpl cpl-sig lib :binary))
         ((probe-file src)
          (values src src-sig lib :source)))))
    
(defun include-path-relative-pathname (spec)
  (let* ((ipath (include-path spec))
         (cpt   (or (include-path-components ipath)
                    ;+include-library-include-path-components+
                    )))
    ;(assert cpt () "Invalid include path specifier ~A." spec)
    (make-pathname :directory `(:relative ,@(mapcar #'string-downcase (butlast cpt)))
                   :name (ifit (first (last cpt))
                             (string-downcase it))
                   :type (ifit (include-path-type ipath)
                             (string-downcase it)))))

(defun make-include-pathname (ipath &optional filetype (subdir :source))
  (let* ((ipath      (include-path ipath))
         (filetype   (or filetype (include-path-type ipath)))
         (lib        (ifit (ensure-library ipath) it
                       (error "Include path ~A refers to an unknown library named ~A."
                              ipath
                              (include-path-library-name ipath))))
         (libsubpath (ecase subdir
                       (:source lib)
                       (:bin    (library-compiled-dir lib)))))
   (values (merge-pathnames
            (merge-pathnames (include-path-relative-pathname ipath)
                             libsubpath)
            (make-pathname :type filetype))
           lib)))

;;;
;;; INCLUDE-PATH PARSER
;;;
(defun @stringp (o)
  (and (stringp o)
       (> (length o) 0)
       (char= (char o 0) #\@)))

(deftype @string ()
  `(satisfies @stringp))


(defun include-path (spec &optional default-type)
  (typecase spec
    (include-path  spec)
    (null          nil)
    (string        (let+ (((lcpts stype)  (tokenize-ipath-spec spec)))
                     (include-path lcpts (or stype default-type))))
    (list          (let ((normal (mapcar #'string-upcase spec)))
                     (if (@stringp (first normal))
                       (parse-symbolic-include-path-spec normal default-type)
                     (let+ ((lib  (first normal))
                            (cpts (rest normal)))
                       (make-include-path lib cpts default-type)))))
    (symbol        (include-path (symbol-name spec)))    
;    (symbol        (parse-symbolic-include-path-spec spec))
    (package       (include-path (package-name spec)))
    (pathname      (include-path-from-pathname spec))))

(defun include-path-valid-p (spec)
  (ignore-errors (include-path spec)))

(defun tokenize-ipath-spec (spec)
  "Input is a string specifier.  
   Output is two values: tokens, type
    Tokens = strings delimited by #\/
    Type = file type"
  (let+ ((tok (tok #\/ spec))
         (name+type   (first (last tok)))
         ((name type) (if name+type
                          (split name+type
                                 (position #\. name+type :test #'safe-char=)
                                 1))))
    (setf (first (last tok)) name)
    (values tok
            type)))

(defun include-path-to-list (ipath)
  (values (cons (include-path-library-name ipath) (include-path-components ipath))
          (include-path-type ipath)))

(defun parse-symbolic-include-path-spec (cpt type)
  (let ((head        (first cpt)))
    (labels ((listener-error (&optional e) 
               (error "Cannot determine include path specifier ~A in Listener~@[: ~A~] " (spec) e))
             (cur () (handler-case (compute-current-include-path)
                       (error (e) (listener-error e))))
             (cur-path () (or *load-truename* *compile-file-truename*
                              (listener-error)))
             (relipath (ipath sub)
               (let+ ((cpts    (include-path-to-list ipath)))
                 (include-path (resolve-ipath-traversal cpts sub) type)))
             (relpath (p sub)
               (merge-pathnames (make-pathname :directory `(:relative ,@sub)) p))
             (spec () (format nil "~A~@[.~A~]" (apply #'delim-mkstr #\/ cpt) type)))
      (test (#'string= head)
        ("@FILE"          (relipath (cur) (rest cpt)))
        ("@LIBRARY"       (relipath (include-path-library-name (cur)) (rest cpt)))
        ("@FOLDER"        (relipath (include-path-parent (cur)) (rest cpt)))
        (("@" "@PACKAGE") (relipath (include-path (or *compile-package* *package*)) (rest cpt)))
        ("@FILE-PATHNAME"     (cur-path) 
                              (assert (not (rest cpt)) ()
                                "~S is invalid.  Children cannot be appended to @FILE-PATHNAME."
                                (spec)))
        ("@FOLDER-PATHNAME"   (relpath (cur-path) (rest cpt)))
        ("@FILE-NAME"     (relipath (pathname-name (cur-path)) (rest cpt)))
        ("@FOLDER-NAME"   (relipath (last (pathname-directory (cur-path)))
                                    (rest cpt)))
        (t
         (let ((shead (subseq head 1)))
           (cond
            ((ipath-traversal-signal-p shead) 
             (relipath (include-path (or *compile-package* *package*))
                       (cons shead (rest cpt))))
            (t                          
             (error "Invalid include path specifier ~A. ~
                     Symbolic path specifiers must be one of @FILE @LIBRARY @FOLDER @PACKAGE ~
                     @FILE-PATHNAME @FOLDER-PATHNAME @FILE-NAME or @FOLDER-NAME." (spec))))))))))


(defun ipath-traversal-signal-p (x)
  (and (stringp x)
       (let ((head (char x 0)))
         (or (char= head #\<)
             (char= head #\>)))))

(defun ipath-up-signal-p (x)
  (and (stringp x) (not (zerop (length x))) (char= (char x 0) #\<)))

(defun ipath-down-signal-p (x)
  (and (stringp x) (not (zerop (length x))) (char= (char x 0) #\>)))

(defun resolve-ipath-traversal (root sub)
  "Implements the logic for pasting sub (a list of strings) to root, accounting if necessary
for traversal - when the first string of sub contains < or > chars."
  (let ((first (first sub)))
    (flet ((bad-char-error (c)
             (error "Unexpected character ~S in relative include-path specifier ~A"
                        c first))
           (truncated-root-and-sub (root-size)
             (when (> root-size (length root))
               (error "~A contains too many ~A characters." first (char first 0)))
             (let ((subseq (subseq root 0 root-size)))
               (append subseq (rest sub)))))
      (cond
       ((null sub) root)

       ((ipath-up-signal-p first)
        (truncated-root-and-sub
         (- (length root)
            (count-if (lambda (x) (if (char= #\<) t (bad-char-error x)))
                      first))))

       ((ipath-down-signal-p first)
        (truncated-root-and-sub 
         (count-if (lambda (x) (if (char= #\>) t (bad-char-error x))) first)))
       
       (t (append root sub))))))


(defun pathname-file-type (pathname)
  (let ((typestr    (pathname-type pathname)))
    (cond ((string-equal typestr +b-compiled-file-type+) :binary)
          ((string-equal typestr +b-source-file-type+) :source))))


;;;
;;; PRINTER and READER:
;;;


(defmethod print-object ((ip include-path) stream)
  (cond
   ((and (null *print-escape*)
         (null *print-readably*)) (princ (include-path-spec ip) stream))
   (t                             (format stream "#0I~S" (include-path-spec ip)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun include-path-reader (stream macro-char signal)
  (declare (ignorable macro-char disp-char))
  (let ((ipath  (include-path (with-dummy-b-package (read stream t nil t)))))
    (ecase signal
      ((NIL) (when ipath (format nil "~A" ipath)))
      (0     ipath)
      (1     (map 'string (lambda (c) (case c (#\/ #\_) (#\. #\_) (t   c))) (include-path-spec ipath)))
      (2     (find-library ipath))
      (3     (include-path-package ipath t))
      (4     (include-path-source-file ipath))
      (5     (include-path-compiled-file ipath)))))


(set-dispatch-macro-character #\# #\I 'include-path-reader +b-readtable+)

#+clisp
(set-dispatch-macro-character #\# #\I 'include-path-reader +b-standard-tokens-readtable+)
)
;;;
;;; MISC MACROS:
(defmacro with-load-ipath ((include-path &optional file)
                           &body body)
  (let ((ipath (gensym "IPATH"))
        (fvar  (gensym "FILE")))
    `(let* ((,ipath  (include-path ,include-path))
            (,fvar    (or ,file (include-path-source-file ,ipath)))
            (*package* (include-path-package ,ipath t)))
       (with-load-path ,fvar
         ,@body))))
