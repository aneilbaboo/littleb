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

;;; File: utility
;;; Description: general utilities for use with common lisp
;;;

;;; $Id: utility.lisp,v 1.11 2008/01/12 16:03:57 amallavarapu Exp $

(in-package mallavar-utility)

;;
;;  MISC UTILITY FUNCTIONS
;;       
(defmethod delete-package-tree ((p symbol))
  (delete-package-tree (find-package p)))

(defmethod delete-package-tree ((p string))
  (delete-package-tree (find-package p)))

(defmethod delete-package-tree ((p package))  
  (let ((used-by (package-used-by-list p)))
    (mapcar #'delete-package-tree used-by)
    (do-symbols (s p)
      (unintern s p))
    (delete-package p)))


(defun find-extra (seq1 seq2 &key (test #'eql) (key #'identity))
  "Finds the first element of seq1 which cannot be paired with an element of seq2.  Elements are tested using a binary predicate given in TEST (default is EQL)"
  (cond 
   (seq1 (let* ((el   (elt seq1 0))
                (pos  (position el seq2 :test test :key key)))
            (cond 
             (pos      (find-extra (subseq seq1 1) (remove-position pos seq2) :test test :key key))
             (t        (values el t)))))

   (t    nil)))
      
(defun remove-position (pos seq)
  (let ((front (subseq seq 0 pos))
        (back  (subseq seq (1+ pos))))
    (cond 
     ((and front back) (concatenate (type-of seq) front back))
     (front            front)
     (back             back))))

(defun split (seq pos &optional (exclude-n 0))
  "Splits sequence at position POS, returning left and right hand sides, excluding EXCLUDE-N elements  - if POS is NIL, returns SEQ and NIL"
  (if pos
      (let* ((lhs  (subseq seq 0 pos))
             (rhs  (subseq seq (+ exclude-n pos))))
        (values lhs rhs))
    (values seq nil)))

(defun assoc-to-list (assoc)
  (list (car assoc) (cdr assoc)))


(defun assoc-val (item assoc &key default (test #'eql) test-not (key #'identity))
  (let ((cons (cond
               (test-not (assoc item assoc :key key :test-not test-not))
               (t        (assoc item assoc :key key :test test) ))))
    (if cons (cdr cons) default)))

(defun show-hash (h)
  (maphash (lambda (s v) (format t "~S ~S~%" s v)) h))
       
(defun copy-hash-table (ht)
  (let ((ht-copy (make-hash-table :test (hash-table-test ht)
                                  :size (hash-table-size ht)
                                  :rehash-size (hash-table-rehash-size ht)
                                  :rehash-threshold (hash-table-rehash-threshold ht))))
    (maphash (lambda (k v)
               (setf (gethash k ht-copy) v))
             ht)
    ht-copy))

(defun quoted-p (o)
  (and (consp o)
       (eq (first o) 'quote)))

(defun ensure-cons (o)
  (if (not (consp o)) (list o)
    o))

(defun ensure-list (o)
  (if (not (listp o)) (list o) o))

(defmacro make-type-sort-predicate (pred-list &key same-type-predicate stable-sort)
  "Provides a binary sort function which sorts objects using an ordered list of predicates.
E.g., (sort '(2 (A B) B A 1) (make-type-sort-predicate (numberp symbolp listp))) => (1 2 A B (A B)).
Objects within a type may be arbitrarily reordered.  To prevent this, the stable-sort argument should be set to T.  E.g., (make-type-sort-predicate (numberp symbolp listp) :stable-sort t)"
  (let* ((a1 (gensym "A1"))
         (a2 (gensym "A2"))
         (ts-code `(or ,@(reverse 
                          (maplist 
                           (lambda (tests)
                             `(and (,(car tests) ,a1)
                                   ,@(if stable-sort
                                         `((not (,(car tests) ,a2))))
                                   ,@(mapcar 
                                      (lambda (not-test)
                                        `(not (,not-test ,a2)))
                                      (cdr tests))))
                           (reverse pred-list))))))
    `(lambda (,a1 ,a2)
       ,(if same-type-predicate
            `(if (eq (type-of ,a1) (type-of ,a2))
                 (,same-type-predicate ,a1 ,a2)
               ,ts-code)
          ts-code))))


;;;; (defsetf find (item seq-place &key new-pos from-end (test #'eql) test-not (start 0) end key) (val)
;;;;   (let ((pos (gensym "POS")))
;;;;     `(let ((,pos (position ,item ,seq-place
;;;;                         :from-end ,from-end :test ,test 
;;;;                         :test-not ,test-not :start ,start 
;;;;                         :end ,end :key ,key)))
;;;;        (cond
;;;;         (pos                 (setf (elt ,sequence ,pos) ,val))
;;;;         ((eq new-pos :start) (push ,item ,seq-place ))
;;;;         ((eq new-pos :end)   
         

(defun find-duplicates (l sort-pred eq-pred)
  "Returns a list of duplicate objects, given a sorting and equality predicate."
  (let ((sorted       (sort l sort-pred)))
    (remove-if #'null 
               (maplist (lambda (subl)
                          (if (funcall eq-pred
                                       (car subl) (cadr subl))
                              subl))
                        sorted))))

(defun bool (p)
  "Converts all non-NIL => T, NIL => NIL"
  (if p t nil))

(defun careful-expt (n p)
  "Attempts to avoid exponentiation errors - good for computing
things like (expt 1000 1/3), which in many implementations yields values
like 9.999999999999998 instead of 10.  
Note: 1) complex values (in n or p) are not carefully exponentiated: 
the implementation's EXPT function is used.
      2) Expect this function to perform much more slowly than the 
implementation EXPT, which is likely to use a fast hardware-based
mechanism."
  (labels ((expt-by-base-type (base power)
             (typecase base
               (integer (let* ((fexpt (expt base power))
                               (rexpt (round fexpt)))
                          (if (and (not (zerop rexpt))
                                   (= (expt rexpt (/ 1 power)) base))
                              rexpt fexpt)))
               (ratio   (let ((rbase (rational base)))
                          (/ (careful-expt (numerator rbase) power)
                             (careful-expt (denominator rbase) power))))
               (real    (coerce (expt-by-base-type (rational base) power) 'real))
               (t       (expt base power)))))
    (typecase p
      (complex    (expt n p))
      (t 
       (let ((rp (rational p)))
         (cond
          ((plusp rp)    (expt-by-base-type n rp))

          ((zerop rp)    1)

          (t             (/ 1 (expt-by-base-type n (* -1 (the rational rp)))))))))))


;; anaphoric if - ala paul graham
;;;; (defmacro ifit (test then &optional else)
;;;;   "Anaphoric if - the symbol 'it' may be used to refer to the result."
;;;;   (let ((result (gensym "RESULT")))
;;;;     `(let ((,result ,test))
;;;;        (declare (special it))
;;;;        (symbol-macrolet ((it ,result))
;;;;          (if ,result ,then ,else)))))

(defun find-and-replace (old new seq &key (start 0) end from-end (test 'eql))
  "Returns 2 values - the string with replacements, and the position in the new string
immediately following the replacement, or NIL if no substitution was made."
  (let* ((pos (search old seq :start2 start :end2 end :from-end from-end :test test))
         (lenold (length old))
         (raw-type (type-of seq))
         (type     (if (consp raw-type) (first raw-type) raw-type)))
    (cond 
     (pos  (multiple-value-bind (left right) (mutils:split seq pos lenold)
             (values (concatenate type left new right) 
                     (- (+ 1 pos (length new)) lenold))))
     (t    (values seq nil)))))


(defun find-and-replace-all (old new seq)
  (loop for (newseq pos) = (multiple-value-list (find-and-replace old new seq)) then
                           (multiple-value-list (find-and-replace old new newseq :start pos))
        while pos
        finally (return newseq)))

(defmacro ifit (test then &optional else)
  "Anaphoric if - the symbol 'it' may be used to refer to the result."
  (let ((itvar (intern "IT")))
  `(let* ((,itvar ,test))
     (symbol-macrolet 
         ,(unless (eq (symbol-package itvar)
                     #.*package*)
            `((it ,itvar)))
       (declare (ignorable ,itvar))
       (if ,itvar ,then ,else)))))

(defmacro whenit (test &body body)
  `(ifit ,test
       (progn ,@body)))

(defmacro condit (&rest clauses)
  (if clauses
      (let* ((current (first clauses))
             (else    (if (rest clauses) `(condit ,@(rest clauses))))
             (test    (first current))
             (body    (rest current)))
        `(ifit ,test
             (progn ,@body)
           ,else))))

(defmacro collect-symbols (packages &rest symbol-types)
  `(with-package-iterator (next-symbol (or ,packages (list-all-packages)) ,@symbol-types)
     (loop with results = nil
           do (let+ (((more? symbol) (next-symbol)))
                (if more? 
                    (push symbol results)
                  (return results))))))

(defun pathname-descendent-p (maybe-child parent &optional (strict nil))
  "Tests whether the pathname maybe-child is a descendent of parent.  If STRICT (default = NIL) is T, the
 same pathname is not considered a descendent.  Returns either NIL or RELATIVE-PATH."
  (let ((maybe-child (normalize-pathname maybe-child))
        (parent      (normalize-pathname parent)))
    (and (equalp (pathname-host maybe-child) (pathname-host parent))
         (equalp (pathname-device maybe-child) (pathname-device maybe-child))
         (and (not (pathname-name parent)) (not (pathname-type parent)))
         (let* ((pd    (pathname-directory parent))
                (cd    (pathname-directory maybe-child))
                (pdlen (length pd))
                (cdlen (length cd)))
           (and (if strict (> cdlen pdlen)
                  (>= cdlen pdlen))
                (loop for elt in pd
                      for child-elt in cd
                      unless (equalp elt child-elt)
                      do (return nil)
                      finally (return (make-pathname :directory `(:relative ,@(subseq cd pdlen))))))))))



(defun pathname-parent (p)
  (cond 
   ((not (pathname-directory-p p)) (make-pathname :host (pathname-host p)
                                                  :device (pathname-device p)
                                                  :directory (pathname-directory p)))
   ((> (length (pathname-directory p)) 1) 
    (make-pathname :device (pathname-device p)
                   :host (pathname-host p)
                   :directory (butlast (pathname-directory p))))))

(defun pathname-ancestors (p)
  (let ((parent (pathname-parent p)))
    (if parent 
        (cons parent 
              (pathname-ancestors parent)))))

(defun pathname-directory-p (pathname)
  (flet ((undefinedp (o) (or (eq o :unspecific) (null o))))
    (ignore-errors
      (let ((p (pathname pathname)))
        (and (undefinedp (pathname-name p))
             (undefinedp (pathname-type p)))))))

(defun pathname-equal (p1 p2)
  (equalp (normalize-pathname p1) (normalize-pathname p2)))

(defun normalize-pathname (p)
  "Replaces any :UNSPECIFIC values with NIL, which ensures that EQUALP tests correctly."
  (flet ((normalize (o) (unless (eq o :unspecific) o)))
    (make-pathname :host (normalize (pathname-host p))
                   :device (normalize (pathname-device p)) 
                   :directory (normalize (pathname-directory p))
                   :name (normalize (pathname-name p))
                   :type (normalize (pathname-type p))
                   :version (let ((n (normalize (pathname-version p))))
                              #+Win32 (unless (eq n :newest) n)
                              #-Win32 n))))

(defun delete-directory (pathname &optional recursive error)
  "Deletes the pathname, and optionally all contained files and folders"
  (handler-case 
      (progn (when recursive
               (let ((subpaths (nconc (mapcar #'normalize-pathname 
                                              (directory (merge-pathnames "*.*" pathname)))
                                      #+:clisp (directory (merge-pathnames "*/" pathname))
                                      #-:clisp nil)))
                 (loop for path in subpaths
                       if (pathname-directory-p path)
                       do (delete-directory path t)
                       else
                       do (delete-file path))))
        #+:lispworks (lw:delete-directory pathname)
        #+:clisp     (ext:delete-dir pathname)
        #+:sbcl      (sb-posix:rmdir pathname)
        #-(:or :sbcl :clisp :lispworks)  (delete-file pathname); hope for the best
        t)
    (error (e) (if error (error e)))))

(defun make-child-pathname (parent relative)
  "Appends path information contained in RELATIVE to PARENT.  Device, host in relative are ignored.  The :ABSOLUTE indicator, if present in RELATIVE, is ignored."
  (flet ((fix-parent-pathname (p)
           (let* ((name (pathname-name p))
                  (type (pathname-type p))
                  (extra-dir (list (format nil "~[~A~;~1*~]~[.~A~;~1*~]"
                                           (if name 0 1) name
                                           (if type 0 1) type))))
             (make-pathname :host (pathname-host parent)
                            :device (pathname-device parent)
                            :directory `(,@(pathname-directory parent)
                                         ,@extra-dir)))))

    (let ((parent (fix-parent-pathname (pathname parent)))
          (relative (pathname relative)))
      (make-pathname :host (pathname-host parent)
                     :device (pathname-device parent)
                     :directory (append (pathname-directory parent)
                                        (rest (pathname-directory relative)))
                     :name (pathname-name relative)
                     :type (pathname-type relative)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun declare-form-p (o)
  (and (consp o) (eq (first o) 'declare))))

(defmacro let+ (bindings &body body)
  "Like LET*, but also allows bindings to be multiple-value-bindings. 
E.g., (let+ ((a     1) 
             ((b c) (values (+ 1 a) 3))
             (d     (+ 1 c)))
        (list a b c d)) 
      => (1 2 3 4)
      This macro respects TYPE, IGNORE, IGNORABLE and SPECIAL DECLARTIONS."
  (labels ((add-decl-spec-to-table (dspec dtable)
             (let ((declaration (first dspec))
                   (decl-data   (rest dspec)))
               (case declaration
                 ((ignore ignorable special) (dolist (s decl-data)
                                               (pushnew `(,declaration) (gethash s dtable))))
                 (type                (let ((type (first decl-data))
                                            (symbols (rest decl-data)))
                                        (dolist (s symbols)
                                          (pushnew `(type ,type) (gethash s dtable))))))))
           ;;;;                  (otherwise          (warn "Ignoring (DECLARE ~S) in LET+.  Only IGNORE, IGNORABLE, SPECIAL and ~
           ;;;;                                              TYPE declarations are meaningful." dspec)))))
           (extract-declares (code)
             (loop with dtable = (make-hash-table)
                   for rest on code
                   for form = (first rest)
                   until (not (declare-form-p form))
                   do (dolist (dspec (rest form))
                        (add-decl-spec-to-table dspec dtable))
                   finally (return (values dtable rest))))
           (declarations (b dtable) ; computes the declaration forms for bindings, b
             (if (consp b) (mapcan (lambda (s) (declarations s dtable)) b)
               (mapcar (lambda (d) `(declare (,@d ,b)))
                       (gethash b dtable))))
           (compute-let+-code (bindings dtable body)
             (let* ((current-binding (first bindings))
                    (var             (if (consp current-binding)
                                         (first current-binding)
                                       current-binding))
                    (var-form        (if (consp current-binding)
                                         (second current-binding)))
                    (next-bindings   (rest bindings))
                    (declarations    (declarations var dtable))
                    (code-body       (if next-bindings 
                                         `(,(compute-let+-code (rest bindings) dtable body))
                                       body)))
               (cond 
                ((consp var) `(multiple-value-bind ,var ,var-form
                                ,@declarations
                                ,@code-body))
                (t           `(let (,current-binding) 
                                ,@declarations
                                ,@code-body))))))
    (multiple-value-bind (dtable true-body) (extract-declares body)
      (compute-let+-code bindings dtable true-body))))
      
(defun copy-file (source target) 
  "Shamelessly copied from Lauri Siponen's post in google groups"
  (with-open-file (in source 
                   :direction :input) 
    (with-open-file (out target 
                     :direction :output 
                     :if-exists :supersede) 
      (loop 
          with buffer = (make-string (* 64 512)) 
          for n = (read-sequence buffer in) 
          until (= n 0) 
          do (write-sequence buffer out :end n))))) 

(defmacro print-class-unreadably (type)
  (let ((o (gensym "OBJ"))
        (stream (gensym "STREAM")))        
  `(defmethod print-object ((,o ,type) ,stream)
     (print-unreadable-object (,o ,stream :type t :identity t)))))
#+Lispworks
(editor:setup-indent 'ifit 2 2 4)
          
(defun contains-duplicates (l)
  "Returns t if the list contains duplicate entries"
  (if l ; if not empty
      (if (find (car l) (cdr l))   
          t
        (contains-duplicates (cdr l)))))  

(defun string-invertcase (s)
  (map 'string (lambda (o)
                 (cond ((upper-case-p o) (char-downcase o))
                       ((lower-case-p o) (char-upcase o))
                       (t o))) s))

(defun symbol-external-p (s &optional (package (symbol-package s)))
  (let ((sname (etypecase s
                 (symbol (symbol-name s))
                 (string s))))
      (multiple-value-bind (the-symbol status)
          (find-symbol sname package)
        (and the-symbol (eq status :external)))))

(defun delim-mkstr (delimiter &rest args)
  "Make a string, delimiting the tokens with the delimiter.  E.g., (delim-mkstr #\. 'a 'b 'c) => A.B.C"
  (with-output-to-string (s)
    (princ (car args) s)
    (dolist (a (cdr args))
      (princ delimiter s)
      (princ a s))))

(defun delim-sym (delimiter &rest args)
  (intern (apply #'delim-mkstr delimiter args)))

(defun mkstr (&rest args)
  "Concatenates all the printed forms of the passed objects to produce a string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun sym (&rest args)
  "Creates a symbol from the printed representation of the passed objects."
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defmethod key ((x symbol))
  "Takes the symbol x and returns the symbol :x (the symbol with the same name in the keyword package)"
  (intern (symbol-name x) #.(find-package "KEYWORD")))

(defmethod key ((x string))
  (intern x #.(find-package "KEYWORD")))

(defun maptree (fun tree)
  "Recursively traverses tree, applying fun to each element which is not a list."
  (flet ((xform (s) 
           (if (listp s) (maptree fun s) (funcall fun s))))
    (mapcar #'xform tree)))

(defun mapatoms (fn tree
                  &key
                  (return :tree) 
                  (list-test (constantly t)))
  "Applies FN to every non-list in TREE.  Return may be one of :LIST :TREE, or NIL.
Only values which pass LIST-TEST are included in the list (default is to include every returned value in the final list). FN is a function which takes the atom (A) as an argument and which returns VALUE, SPLICE - where splice indicates that A is a list to be spliced."
  (labels ((includep (o)
             (funcall list-test o))
           (map-fn (obj)
             (cond              
              ((atom obj) (funcall fn obj))
              
              (t
               (let+ ((car                  (car obj))
                      ((car-val car-splice) (map-fn car))
                      (cdr                  (cdr obj))
                      ((cdr-val cdr-splice) (if cdr (map-fn cdr))))
                 (if cdr-splice
                     (setf cdr-val (append (first cdr-val) (rest cdr-val))))
                 (case return
                   ((:tree 'tree)  (values (cons car-val cdr-val) car-splice))
                   ((:list 'list)  (let* ((incl-car (includep car-val))
                                          (incl-cdr (includep cdr-val))
                                          (cdr-val  (when incl-cdr 
                                                      (if (listp cdr) cdr-val (list cdr-val)))))
                                     (cond
                                      ((and incl-car (consp car))
                                       (nconc car-val cdr-val))
                                      (incl-car 
                                       (cons car-val cdr-val))
                                      (t cdr-val))))
                   ((nil)  nil)))))))
    (map-fn tree)))

(defun keyword-list-p (list)
  "Tests whether the list is of the form (:KEY1 VALUE1 :KEY2 VALUE2 ...)"
  (every #'(lambda (a) (keywordp (car a)))
          (assoc-list-from-plist list)))

(defun parse-key-list (keys input)
  "Collects all values after a keyword and associates them with the keyword. If NIL appears in keys, then any initial values (before other keywords) are stored.
   E.g., (parse-key-list '(NIL :a :b :c) '(x y z :a 1 2 :b 3 :c (4 5) :a 6 :d 7))
     => ((NIL . '(x y z)) (:a . (1 2 6 :d 7)) (:b . (3)) (:c . ((4 5))))"
  (let* ((assoc-list (mapcar (lambda (k) (cons k ())) keys))
         (current-assoc (assoc nil assoc-list)))
    (loop for item in input
          if (member item keys)
          do (setf current-assoc (assoc item assoc-list))
          else do (push item (cdr current-assoc)))
    (mapcar (lambda (assoc) 
              (cons (car assoc)
                    (reverse (cdr assoc))))
            assoc-list)))

(defun maphash-to-list (ht &optional fn)
  "ht is a hash-table, list-form may be one of :assoc-list :plist :keys :values"
  (loop with fn = (or fn (lambda (k v) (cons k v)))
        for k being the hash-keys of ht
        for v being the hash-values of ht
        collect (funcall fn k v)))

(defun mapp (fn list)
  "Maps fn over list, returning list (mapp/map-plist is analogous to mapc/mapcar)"
  (when list
    (loop for iter = list then (cdr val-cons)
          for key = (first iter)
          for val-cons = (cdr iter)
          for val = (typecase val-cons
                      (cons (first val-cons))
                      (t    (plist-missing-value-error key)))
          do (funcall fn key val)
          while (cdr val-cons)))
  list)

(defun map-plist (fn list)
  "Iterates over a property list, calling fn (a binary function) on each pair in the list."
  (when list
    (loop for iter = list then (cdr val-cons)
          for key = (first iter)
          for val-cons = (cdr iter)
          for val = (typecase val-cons
                      (cons (first val-cons))
                      (t    (plist-missing-value-error key)))
          collect (funcall fn key val)        
          while (cdr val-cons))))

(defun plist-missing-value-error (key)
  (error "While mapping invalid property list.  No value provided for key ~S" key))


(defun map-alist (fn alist)
  "Iterates over an assoc list, calling fn (a binary function which takes two args, being the car & cdr of each assoc)"
  (mapcar (lambda (a)
            (funcall fn (car a) (cdr a)))
          alist))

(defun assoc-list-from-plist (list)
  (map-plist #'(lambda (k v) (cons k v)) list))

(defmacro pushend (item place)
  "Adds ITEM to the end of LIST"
  `(setf ,place (nconc ,place (list ,item))))

(defmacro pushnew-first (item place &key (test '#'equalp) (key '#'identity))
  `(setf ,place (add-new-elt-first ,item ,place :test ,test :key ,key)))

(defun add-new-elt-first (item list &key (test 'equalp) (key 'identity))
  (ifit (position (funcall key item) list :test test :key key)
      (cons item (remove-position it list))
    (cons item list)))
;;;; (defun tok (token sequence)
;;;;   "Separates sequence into subsequences separated by tok."
;;;;   (let ((p (append (list -1) (positions token sequence))))
;;;;     (append (mapcar #'(lambda (start end)
;;;;                         (subseq sequence (1+ start) end))
;;;;                     p (cdr p))
;;;;             (list (subseq sequence (1+ (car (last p))))))))

;; more efficient tok:
(defun tok (tokenizers seq &optional test)
  (let* ((test (or test (if (stringp seq) #'char= #'eql) 'eql))
         (unary-test (if (listp tokenizers)
                         (lambda (o) (member o tokenizers :test test))
                       (lambda (o) (funcall test tokenizers o)))))
                 
    (etypecase seq 
      (array
       (loop with collector = (make-array (length seq) :element-type (array-element-type seq) :fill-pointer 0)
             for elt across seq
             if (funcall unary-test elt)
             collect (copy-seq collector) into toks
             and do (setf (fill-pointer collector) 0)
             else do (vector-push elt collector)
             finally (return (nconc toks (list (copy-seq collector))))))
      (list
       (loop with collector = nil
             for elt in seq
             if (funcall unary-test elt)
             collect (nreverse collector) into toks
             and do (setf collector nil)
             else do (push elt collector)
             finally (return (nconc toks (list (nreverse collector)))))))))

(defun positions (item sequence &key (start 0))
  "Returns a list containing the 0-based positions where item is found in sequence"
  (let ((new-pos (position item sequence :start start)))
    (if new-pos
        (nconc (list new-pos)
               (positions item sequence  :start (+ 1 new-pos))))))

(defun unprefix (prefix symbol) 
  "Removes the prefix from the printed representation of symbol, and returns the symbol with the resulting name.  If symbol doesn't contain the prefix, then symbol is returned unmodified.  E.g., (unprefix 'AB 'ABCD) => CD"
  (let* ((prefix-str (if (stringp prefix) prefix
                       (format nil "~S" prefix)))
         (symbol-str (if (stringp symbol) symbol
                       (format nil "~S" symbol)))
         (prefix-len (length prefix-str))
         (prefix-exists (and (eq (search prefix-str symbol-str) 0)
                             (> prefix-len 0))))
    (if prefix-exists
        (intern (subseq symbol-str prefix-len))
      symbol)))

(defun xor (a1 a2)
  (and (or a1 a2)
       (not (and a1 a2))))

;(defun external (&optional (package *package*) (type :symbols))
;  "Retrieves all external symbols from package.  Type may be :symbols or :functions."
;  (let (result
;        (pkg    (if (packagep package) package (find-package package))))
;    (reverse (do-external-symbols (s pkg result)
;               (if (or (eq type :symbols)
;                       (and (eq type :functions)
;                           (function-information s)))
;                   (push s result))))))

(defmacro fn ((&rest ll) &body body)
  "FN = LAMBDA.  Returns a function (FN (ARGS) BODY) == (LAMBDA (ARGS) BODY)"
  `(lambda ,ll ,@body))

(defmacro mvbind (vars values-form &body body)
  "Alias for multiple-value-bind"
  `(multiple-value-bind ,vars ,values-form ,@body))

(defmacro mvlist (form)
  "Alias for multiple-value-list"
  `(multiple-value-list ,form))

(defmacro mvprog1 (form &body body)
  "Alias for multiple-value-prog1"
  `(multiple-value-prog1 ,form ,@body))

(defmacro mvcall (func &body body)
  "Alias for multiple-value-call"
  `(multiple-value-call ,func ,@body))

(defmacro mvsetq (variables form)
  "Alias for multiple-value-setq"
  `(multiple-value-setq ,variables ,form))



(defun sub-pathname (parent &rest children)
  "Constructs a pathname in which child represents a folder, file or tree beneath parent.
parent and child may be strings or pathname objects"
  (let* ((p-path (if (pathnamep parent) parent (pathname parent)))
         (child  (first children))
         (c-path (if (pathnamep child) child (pathname child)))
         (p-dirs (append (pathname-directory p-path)
                         (ifit (pathname-name p-path)
                             (list it))))
         (c-dirs (pathname-directory c-path))
         (dir    (append p-dirs (if (keywordp (first c-dirs)) (rest c-dirs) c-dirs)))
         (newp   (make-pathname :host (pathname-host p-path)
                                :device (pathname-device p-path)
                                :directory dir
                                :name (pathname-name c-path)
                                :type (pathname-type c-path)
                                :version (pathname-version p-path))))
    (ifit (rest children)
        (sub-pathname newp it)
      newp)))
                           


(defun all-positions (item seq &key (start 0) (test #'eq) (key #'identity))
  (loop for pos from start to (1- (length seq))
        for o = (elt seq pos)
        when (funcall test item (funcall key o))
        collect pos))
;;;;
;;;;
;;;; LET*PROTECT - sequentially binds variables, providing guaranteed cleanup 
;;;;                and error detection capabilities
;;;;
;;;; FORMAT:

(defmacro let*protect (bindings &body body)
  "Sequentially binds variables, providing guaranteed cleanup and error detection capabilities.
USAGE: (let*protect bindings forms*)
where bindings = list of (symbol value-form cleanup-form error-form)
      cleanup-form is guaranteed to be executed if the binding succeeds
      error-form contains code which is executed if the binding fails
                 = (ESYM FORMS*) where esym is a symbol which holds the error object
                 and FORMS are the forms to be executed in event of error

      (let*protect ((symbol val)) body) = (let ((symbol val)) body)
      (let*protect ((symbol val cleanup-form)) body) = (let ((symbol val)) 
                                                         (unwind-protect body
                                                           cleanup-form))
      (let*protect ((symbol val cleanup-form (esym error-body))) body)
      = (handler-case (let ((symbol val)) (unwind-protect body cleanup-form))
          (error (esym) error-body))"
  (compute-let*protect bindings nil body))

(defun compute-let*protect (bindings cleanup-form body)
  (cond 
   (bindings (compute-let*protect-unwind-form 
              (compute-let*protect-let-form (first bindings)
                                            (rest bindings)
                                            body)
              cleanup-form))
   (t        (compute-let*protect-unwind-form 
              `(progn ,@body)
              cleanup-form))))

(defun compute-let*protect-unwind-form (protected-form cleanup-form)
  (if cleanup-form
      `(unwind-protect ,protected-form ,cleanup-form)
    protected-form))

(defun compute-let*protect-let-form (binding rest-bindings body)
  (let* ((symbol (first binding))
         (val    (second binding))
         (next-cleanup (third binding))
         (error-form   (fourth binding))
         (error-symbol (car error-form))
         (error-body   (cdr error-form))
         (let-form     `(let ((,symbol ,val))
                          ,(compute-let*protect rest-bindings
                                                next-cleanup
                                                body))))
    (if error-form
        `(handler-case ,let-form
           (error (,error-symbol) ,@error-body))
      let-form)))


(defun compute-choice-paths (clist)
  "Given an ordered list of choices which may be made, a path is one possible selection of choices.  This function returns a list of all possible paths given a list of choices.  
    E.g., (compute-paths '((:a :b) (:c :d))) => ((:a :c) (:a :d) (:b :c ) (:b :d))"
  (let* ((choices        (first clist))
         (rest-clist     (rest clist))
         (rest-paths     (cond
                          (rest-clist (compute-choice-paths rest-clist))
                          (t          (list nil)))))
                           
    (mapcan (lambda (choice)
              (mapcar (lambda (rest-path)
                        (cons choice rest-path))
                      rest-paths))
            choices)))



(defun quick-and-dirty-pairwise-combinations (lst)
  "Not a beautiful algorithm"
  (remove-duplicates
   (mapcon 
    (lambda (sub)
      (let ((first-el (first sub)))
        (mapcar (lambda (second-el)
                  (list first-el second-el))
                (rest sub))))
    lst) :test #'equalp))


(defun all-same (seq &key (key #'identity) (test #'eql))
  "tests whether all elements of seq are the same.  Optional keyword parameters may be provided:
KEY is a unary function which returns the test-value (default is the IDENTITY function).
TEST is a binary test function (default is EQL)."
  (let ((test-val (funcall key (elt seq 0))))
    (every (lambda (o) (funcall test (funcall key o) test-val)) (subseq seq 1))))
    

(defmacro defun-replacement (name args &body body)
  "Defines a replacement function which overrides the original function defined for name.  The original function is available within body as the local function ORIGINAL."
  (let ((replacement-name (sym "%" name "-REPLACEMENT-FUN%"))
        (original-fn-const     (sym "+%" name "-ORIGINAL-FUNCTION%+"))
        (all-args         (gensym "ALL-ARGS")))
    `(eval-when #-(or :lucid :cmu17 :cmu18) (:load-toplevel :execute)
                #+(or :lucid :cmu17 :cmu18) (load eval)

       (unless (boundp ',original-fn-const)
         (defconstant ,original-fn-const
           (symbol-function ',name)))

       (defun ,replacement-name ,args
         (flet ((original (&rest ,all-args) (apply ,original-fn-const ,all-args)))
           ,@body))

       (let (#+(or :mcl (and :CCL (not :lispworks)))
             (ccl:*warn-if-redefine-kernel* nil))
         #-(or (and allegro-version>= (version>= 4 1)) :lispworks)
         (setf (symbol-function ',name)
               (symbol-function ',replacement-name)))
       #+:lispworks
       (let ((warn-packs system::*packages-for-warn-on-redefinition*))
         (declare (special system::*packages-for-warn-on-redefinition*))
         (setq system::*packages-for-warn-on-redefinition* nil)
         (setf (symbol-function ',name)
               (symbol-function ',replacement-name))
	(setq system::*packages-for-warn-on-redefinition* warn-packs))

      #+(and allegro-version>= (version>= 4 1))
      (excl:without-package-locks
       (setf (symbol-function ',name)
	 (symbol-function ',replacement-name)))

      ',name)))


(defun compose (&rest fns)
  (lambda (&rest args)
    (loop for fn in (reverse fns)
          for ret-val = (apply fn args) then (funcall fn ret-val)
        finally (return ret-val))))

(define-compiler-macro compose (&rest fns)
  (let ((args (gensym "ARGS")))
    `(lambda (&rest ,args)
       ,(loop for fn in (reverse fns)
            for ret-val = `(apply ,fn ,args) then `(funcall ,fn ,ret-val)
            finally (return ret-val)))))

(defun parse-fun-declarations (forms)
  (let* ((decl-pos (position-if-not #'stringp forms))
         (code-pos (position-if-not #'declare-form-p forms :start decl-pos)))
    (values (subseq forms 0 code-pos)
            (subseq forms code-pos))))

(defun destructured-method-body-values (method-body)
  (let+ ((pos-args             (position-if #'listp method-body))
         (qualifiers           (unless (and pos-args (eq pos-args 0)) (subseq method-body 0 pos-args)))
         (args                 (nth pos-args method-body))
         (pos-non-spec         (position-if (lambda (o) (member o lambda-list-keywords)) args))
         (specializer-args     (subseq args 0 pos-non-spec))
         (non-specializer-args (if pos-non-spec (subseq args pos-non-spec)))
         ((doc-decl code)      (parse-fun-declarations (nthcdr (1+ pos-args) method-body))))
    (values qualifiers args doc-decl code specializer-args non-specializer-args)))
     
(defmacro destructure-method-body ((qualifiers args doc-decl code
                                    &optional
                                    (specializer-args (gensym)) 
                                    (non-specializer-args (gensym)))
                                   method-body &body body)
  "Given the body of a defmethod form (i.e., everything after the method name), this macro 
destructures the body, binding variables to the method qualifiers, the argument list and the code"
  `(multiple-value-bind (,qualifiers ,args ,doc-decl ,code ,specializer-args ,non-specializer-args)
       (destructured-method-body-values ,method-body)
     (declare (ignorable ,qualifiers ,args ,doc-decl ,code ,specializer-args ,non-specializer-args))
     ,@body))

(defun pair-plist (keys values &optional plist)
  "Produces a property list matching keys to values (in order)"
  (loop for ret-val = plist 
                 then (cons key (cons val ret-val))
        for key in (reverse keys)
        for val in (reverse values)
        finally (return ret-val)))

(defun make-pair-list (l1 l2) 
  (loop for v1 in l1
        for v2 in l2
        collect (list v1 v2)))
        
(defmacro def-multitype-method (name &body body)
  (labels ((specializer-types (s) (if (and (consp s) (> (length s) 1)) (rest s) '(t)))
           (lambda-var-name   (s) (if (consp s) (first s) s))
           (eql-type-p (o) (and (consp o) (eq (first o) 'eql)))
           (convert-to-printable-typelist (typelist)
             (mapcar (lambda (type)
                       (if (eql-type-p type) (format nil "=~S" (second type))
                         type))
                     typelist)))
    (destructure-method-body (quals args doc-decl code spec non-spec) body
      (let* ((specializer-vars  (mapcar #'lambda-var-name spec))
             (type-lists        (compute-choice-paths (mapcar #'specializer-types spec)))
             (arg-lists         (mapcar (lambda (tlist)
                                          (append (make-pair-list specializer-vars tlist)
                                                  non-spec))
                                        type-lists))
             (non-spec-vars     (mapcar #'lambda-var-name (remove-if (lambda (o) (member o lambda-list-keywords)) non-spec)))
             (fun-args          (append specializer-vars non-spec-vars))
             (fun-name          (sym name (format nil "<~{~S~^~~~}><~{<~{~A~^~~~}>~}>~~MULTITYPE~~METHOD" quals 
                                                  (mapcar #'convert-to-printable-typelist type-lists))))
             (methods           (mapcar (lambda (arg-list)
                                          `(defmethod ,name ,@quals ,arg-list
                                             ,@doc-decl
                                             (,fun-name ,@fun-args #'call-next-method)))
                                        arg-lists))
             (x                 (gensym "FIRST"))
             (x-p               (gensym "FIRST-P"))
             (cnm-args          (gensym "REST"))
             (call-next-method-fn (gensym "CNM-FN")))
      `(progn
         (defun ,fun-name (,@fun-args ,call-next-method-fn)
           ,@doc-decl
           (flet ((call-next-method (&optional (,x nil ,x-p) &rest ,cnm-args)
                    (if ,x-p (apply ,call-next-method-fn ,x ,cnm-args)
                      (funcall ,call-next-method-fn))))
             ,@code))
         (list ,@methods))))))

(defmacro def-commutative-operator-method (&whole form name &body body)
  (destructure-method-body (quals args doc-decl code-body) body
    (unless (eq (length args) 2)
      (error
       "Invalid commutative-operator-method lambda list (expecting exactly 2 arguments) in ~S" form))
    (let+ ((a1       (first args))
           (a2       (second args))
           ((arg1 a1-types)     (if (consp a1) (values (first a1) (rest a1)) (values a1 '(t))))
           ((arg2 a2-types)     (if (consp a2) (values (first a2) (rest a2)) (values a2 '(t))))
           (fun-name (sym name (format nil "~{_~A~}~{_~A~}/~{_~A~}" quals a1-types a2-types) "-COMMUTATIVE-OPERATOR-METHOD-BODY"))
           (cnm-a1   (gensym "A1"))
           (cnm-a2   (gensym "A2"))
           (mlist    ())
           (methods  (loop for t1 in a1-types
                           do (loop for t2 in a2-types
                                    do (pushnew (cons (list t1 t2)
                                                      `(defmethod ,name ,@quals ((,arg1 ,t1) (,arg2 ,t2)) 
                                                         ,@doc-decl
                                                         (,fun-name ,arg1 ,arg2 #'call-next-method)))
                                                mlist :key #'car :test #'equalp)
                                    do (pushnew (cons (list t2 t1)
                                                      `(defmethod ,name ,@quals ((,arg2 ,t2) (,arg1 ,t1)) 
                                                         ,@doc-decl
                                                         (,fun-name ,arg1 ,arg2 (lambda (,cnm-a1 ,cnm-a2)
                                                                         (call-next-method ,cnm-a2 ,cnm-a1)))))
                                                mlist :key #'car :test #'equalp))
                           finally (return (mapcar #'cdr mlist)))))
      `(progn
         (defun ,fun-name (,arg1 ,arg2 &optional call-next-method-fn)
           ,@doc-decl
           (flet ((call-next-method (&optional (,cnm-a1 ,arg1) (,cnm-a2 ,arg2))
                    (funcall call-next-method-fn ,cnm-a1 ,cnm-a2)))
             ,@code-body))
         (list ,@methods)))))

(defun insert (element position sequence)
  (splice (list element) position sequence))

(defun splice (new position sequence)
  "Splices sequence NEW into SEQUENCE at POSITION"
  (cond 
   (sequence
    (let ((prefix (subseq sequence 0 position))
          (suffix (subseq sequence position)))
      (concatenate (type-of sequence) 
                   prefix
                   new
                   suffix)))
   ((/= position 0) 
    (error "Index ~S is out of range for sequence ~S." position sequence))
    
   (t new)))


(defmacro timed-dolist ((var list &key 
                             (elapsed-time    '#:elapsed-time)
                             (result-form     elapsed-time)
                             (callback        'default-timed-dolist-callback)
                             (callback-period 1) ; number of iterations between calls to callback
                             (remaining-time  '#:remaining-time)
                             (iterations      '#:iterations))
                        &body body)
  "Calls a function periodically while iterating over list.  
The callback function has 3 arguments: ITERATIONS, ELAPSED-TIME, REMAINING-TIME."
  (let ((list-var '#:list-var)
        (len      '#:len)
        (period   '#:period)
        (time     '#:time)
        (new-time '#:new-time)
        (prev-time '#:prev-time))
    `(loop with ,time = (get-universal-time)
           with ,list-var = ,list
           with ,len = (length ,list-var)
           with ,period = ,callback-period
           for ,var in ,list-var
           for ,iterations = 0 then (1+ ,iterations)
           for ,prev-time = ,time then ,new-time
           for ,new-time    = (get-universal-time)
           for ,elapsed-time = (- ,new-time ,time)
           for ,remaining-time = 0 then (* (- ,len ,iterations)  ; remaining iterations
                                           (+ (/ (- ,new-time ,time)
                                                 ,iterations 2)  ; average time / 2 
                                            (/ (- ,new-time ,prev-time) 2))) ; last time / 2
           when (= (mod ,iterations ,period) 0)
           do (funcall ,callback ,iterations ,elapsed-time ,remaining-time)
           do (progn ,@body)
           finally (return ,result-form))))


(defun decode-absolute-time (time)
  (let+ (((minutes seconds) (floor time 60))
         ((hours minutes)   (floor minutes 60))
         ((days hours)      (floor hours 24))
         ((weeks days)      (floor days 7)))    
    (values (truncate seconds) minutes hours days weeks)))

(defun macroexpand-n (n form &optional env)
  (if (> n 0)
      (macroexpand-n (1- n) (macroexpand-1 form env))
    form))


                                                   
(defun match (pattern sequence single-wild multi-wild
                      &optional
                      (equality-test #'equalp)
                      (empty-test (lambda (o) (eq (length o) 0))))
  "Matches pattern to sequence, allowing for single or multiple wildcard values"
  (labels ((eqt (x y) (funcall equality-test x y))
           (emptyp (o) (funcall empty-test o))
           (internal-match (pattern sequence)
             (let+ ((empty-p  (emptyp pattern))
                    (empty-s  (emptyp sequence))
                    ((phead ptail)   (if empty-p 
                                         (values nil pattern)
                                       (values (elt pattern 0)
                                               (subseq pattern 1))))
                    ((shead stail)   (if empty-s
                                         (values nil sequence)
                                       (values (elt sequence 0)
                                               (subseq sequence 1)))))
               (cond 
                (empty-p                 empty-s)
                ((equalp phead single-wild) (internal-match ptail stail))
                ((equalp phead multi-wild)  (or (emptyp ptail)
                                             (internal-match ptail sequence)
                                             (internal-match ptail stail)
                                             (and (not (emptyp stail))
                                                  (internal-match pattern stail))))
                (t                       (and (eqt phead shead)
                                              (internal-match ptail stail)))))))
    (internal-match pattern sequence)))


(defmacro test ((testform keyform &optional invert) &body body)  
  "Similar syntax to CASE, where all the tests are done with the same 2-value predicate, test.
TESTFORM and KEYFORM are evaluated.  The test is performed with KEYFORM as the first argument
to the function returned by testform if INVERT is nil.  Otherwise, KEYFORM is the second argument.
   e.g.,   (setq x #\d)
           (test (#'char= x)
             ((#\A #\B #\C)  1)
             (#\D            2)
             (t              3)) => 2

   e.g.,   (setq mylist '(z q c))
           (test (#'member mylist t) ; member arglist is (ITEM SEQUENCE ...)
             (a 1)
             (b 2)
             (c 3)) => 3"
  (let ((key          (gensym "KEY"))
        (defined-test (if (and (consp testform) (symbolp (second testform)))
                          (case (first testform)
                            (function (second testform))
                            (quote    (second testform)))))
        (test         (gensym "TEST")))
    (labels ((arglist (a) (if invert (list a key) (list key a)))
             (test-expr (x) 
               (if defined-test 
                   `(,defined-test ,@(arglist x))
                 `(funcall ,test ,@(arglist x))))
             (process-case-body (key-code)
               (unless (consp key-code) (error "~S -- Illegal atomic clause in TEST." key-code))
               (let* ((first (first key-code))
                      (keys (cond 
                             ((eq first t)   t)
                             ((consp first) `(or ,@(mapcar #'test-expr
                                                           (ensure-list first))))
                             (t             (test-expr first)))))
                 `(,keys ,@(rest key-code)))))
      `(let ((,key ,keyform)
             ,@(unless defined-test `((,test ,testform))))
         (cond
          ,@(mapcar #'process-case-body body))))))

(defmacro with-load-path (path &body body)
  (let ((p (gensym "P")))
    `(let* ((,p (if ,path (pathname ,path)))
            (*load-truename* ,p)
            (*load-pathname* ,p)
            *compile-file-truename*
            *compile-file-pathname*)
       ,@body)))


(defmacro with-compile-path (path &body body)
  (let ((p (gensym "P")))
    `(let* ((,p (if ,path (pathname ,path)))
            (*compile-file-truename* ,p)
            (*compile-file-pathname* ,p)
            *load-truename*
            *load-pathname*)
       ,@body)))

(defmacro def-binding-environment (name binding-form)
  (let ((top-level (if (null binding-form) '#:top-level ())))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn (defparameter ,name ',top-level)
         (setf (get ',name :binding-form) ',binding-form
               (get ',name :top-level-binding) ',top-level)))))

(defmacro with-binding-environment ((&rest names) &body body)
  (cond
   (names
    (let+ ((err   '#:error)
           (name (first names))
           (bform (get name :binding-form err))
           (tform (get name :top-level-binding err))
           (at-top-level-p (gensym "AT-TOP-LEVEL-P")))
      (assert (not (or (eq bform err) (eq tform err))) ()
        "(WITH-BINDING-ENVIRONMENT (~S) ...) is invalid.  ~* is not a binding environment."
        name)
      `(let ((,at-top-level-p (eq ,name ',tform)))
         (progv 
             (when ,at-top-level-p  '(,name))
             (when ,at-top-level-p  (list (handler-case ,bform 
                                            (error (e) (error "Error evaluating ~S binding form - ~A" ',name e)))))
           (with-binding-environment (,@(rest names)) 
             ,@body)))))
   (t `(progn ,@body))))


(defun insert-after-if (predicate item list &key (key 'identity) max from-end)
  (loop for rest on (if from-end (reverse list) list)
        for head = (first rest)
        for key-value = (funcall key (first rest))
        unless from-end collect head into newlist
        when (funcall predicate key-value)
         collect item into newlist
         and sum 1 into count
        when from-end collect head into newlist
        if (and max (>= count max))
          append (rest rest) into newlist
          and do (loop-finish)
        finally (return (values (if from-end (nreverse newlist) newlist) count))))

;;;; (defun ninsert-after-if (predicate item list &key (key 'identity) from-end)
;;;;   (loop for rest on (if from-end (reverse list) list)
;;;;         for head = (funcall key (first rest))
;;;;         when (funcall predicate head)
;;;;         do (setf (cdr rest) (cons item (rest rest))
;;;;                  rest (cdr rest))
;;;;         and sum 1 into count
;;;;         finally return (values (if from-end (reverse list) list) count)))
;;;;         
(defun insert-after (predecessor item list &key (test 'eql) (key 'identity) from-end)
  (insert-after-if (lambda (x) (funcall test x predecessor)) item list :key key :from-end from-end))

;;;; (defun ninsert-after (predecessor item list &key (test 'eql) (key 'identity) from-end)
;;;;   (ninsert-after-if (lambda (x) (funcall test x predecessor))  item list :key key :from-end from-end))


;;;
;;; portable code from David Young's LISA:
;;;
(defun gc ()
  "Invoke the garbage collector."
  #+allegro (excl:gc)
  #+clisp (#+lisp=cl ext:gc #-lisp=cl lisp:gc)
  #+cmu (ext:gc)
  #+cormanlisp (cl::gc)
  #+gcl (si::gbc)
  #+lispworks (hcl:normal-gc)
  #+lucid (lcl:gc)
  #+sbcl (sb-ext:gc)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-code (typecase code (number code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code)))
