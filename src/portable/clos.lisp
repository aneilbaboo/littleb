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

(in-package portable)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'mallavar-utility))

#-lispworks
(declaim (inline class-slots))

#-lispworks
(defun class-slots (class)
   #+allegro (clos:class-slots class)
   #+clisp (clos::class-slots class)
   #+cmu (pcl::class-slots class)
   #+cormanlisp (cl:class-slots class)
   ;#+lispworks (hcl::class-slots class)
   #+lucid (clos:class-slots class)
   #+sbcl (sb-pcl::class-slots class))

(defun class-slots1 (class)
  (class-slots class))

(declaim (inline slot-definition-name))

(defun slot-definition-name (slot)
   #+(and allegro (not (version>= 6))) (clos::slotd-name slot)
   #+(and allegro (version>= 6)) (clos:slot-definition-name slot)
   #+clisp (clos::slotdef-name slot)
   #+cmu (slot-value slot 'pcl::name)
   #+cormanlisp (getf slot :name)
   #+lispworks (hcl::slot-definition-name slot)
   #+lucid (clos:slot-definition-name slot)
   #+sbcl (slot-value slot 'sb-pcl::name))

(declaim (inline slot-definition-initargs))

(defun slot-definition-initargs (slot)
  #+(and allegro (not (version>= 6))) (clos::slotd-initargs slot)
  #+(and allegro (version>= 6)) (clos:slot-definition-initargs slot)
  #+clisp (clos::slotdef-initargs slot)
  #+cmu (slot-value slot 'pcl::initargs)
  #+cormanlisp (getf slot :initargs)
  #+lispworks (hcl::slot-definition-initargs slot)
  #+lucid (clos:slot-definition-initargs slot)
  #+sbcl (slot-value slot 'sb-pcl::initargs))

(declaim (inline slot-definition-one-initarg))
(defun slot-definition-one-initarg (slot) (car (slot-definition-initargs slot)))

(defun slot-one-initarg (slot)
  (slot-definition-one-initarg slot))

(declaim (inline slot-definition-allocation))

(defun slot-definition-allocation (slot)
  #+(and allegro (not (version>= 6)))
  (clos::slotd-allocation slot)
  #+(and allegro (version>= 6))
  (clos:slot-definition-allocation slot)
  #+clisp (clos::slotdef-allocation slot)
  #+cmu (pcl::slot-definition-allocation slot)
  #+cormanlisp (getf slot :allocation)
  #+lispworks (hcl::slot-definition-allocation slot)
  #+lucid (clos:slot-definition-allocation slot)
  #+sbcl (sb-pcl::slot-definition-allocation slot))

(defun slot-alloc (slot)
  (slot-definition-allocation slot))

(defun slot-name (slot)
  (slot-definition-name slot))

(defun class-slot-list (class &optional (all t))
  "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
      (unless (class-finalized-p class)
        (finalize-inheritance class))
      (mapcan (if all (compose #'list #'slot-name)
                (lambda (slot)
                  (when (eq (slot-alloc slot) :instance)
                    (list (slot-name slot)))))
              (class-slots1 class)))

(defun copy-instance (o)
  "Returns a shallow copy of the standard class o"
  (loop with new = (make-instance (type-of o))
        for slot in (class-slot-list (class-of o))
        do (setf (slot-value new slot) (slot-value o slot))
        finally return new))

(defun class-slot-initargs (class &optional (all t))
  "Return the list of initargs of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
initargs for all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  (mapcan (if all (compose #'list #'slot-one-initarg)
            (lambda (slot)
              (when (eq (slot-alloc slot) :instance)
                (list (slot-one-initarg slot)))))
          (class-slots class)))

#+(or clisp cmu)
(defun ensure-class (name &key (direct-superclasses '()))
  (eval `(defclass ,name ,direct-superclasses ())))

#+clisp
(defun class-finalized-p (class)
  (declare (ignore class))
  t)

#+lispworks
(defun class-finalized-p (class)
  (harlequin-common-lisp:class-finalized-p class))
#+clisp
(defun finalize-inheritance (class)
  (declare (ignore class))
  (values))
#+lispworks
(defun finalize-inheritance (class)
  (clos:finalize-inheritance class))
(defun is-standard-classp (class)
  (or (eq (class-name class) 'standard-object)
       (eq (class-name class) t)))

(defun find-direct-superclasses (class)
  #+clisp
  (remove-if #'is-standard-classp (clos::class-direct-superclasses class))
  #-clisp
  (remove-if #'is-standard-classp (clos:class-direct-superclasses class)))
             
(defun class-all-superclasses (class-or-symbol)
  (labels ((find-superclasses (class-list superclass-list)
             (let ((class (first class-list)))
               (if (or (null class-list)
                       (is-standard-classp class))
                   superclass-list
                 (find-superclasses 
                  (find-direct-superclasses class)
                  (find-superclasses 
                   (rest class-list) (pushnew class superclass-list)))))))
    (let ((class
           (if (symbolp class-or-symbol)
               (find-class class-or-symbol)
             class-or-symbol)))
      (nreverse (find-superclasses (find-direct-superclasses class) nil)))))



(defun mapclass (fn class)
  "Maps a unary function fn over the class and all of its superclasses."
  (let* ((all-classes   (cons class
                              (class-all-superclasses class))))
    (mapcar fn all-classes)))
       
(defun collect-class-lists (fn class)
  "Maps a unary function fn over the class and superclasses.  Fn must return a list or nil.  The results are returned as a flat list (not a list containing lists)."
  (remove-if #'null (mapclass fn class)))

(defmethod class-direct-subclasses ((class standard-class))
  #+Lispworks
  (hcl:class-direct-subclasses class)
  #-Lispworks
  (clos:class-direct-subclasses class))

(defmethod class-direct-superclasses ((class standard-class))
  #+Lispworks
  (hcl:class-direct-superclasses class)
  #-Lispworks
  (clos:class-direct-superclasses class))

(defmethod class-direct-subclasses ((class symbol))
  (class-direct-subclasses (class-of class)))

(defmethod class-direct-superclasses ((class symbol))
  (class-direct-superclasses (class-of class)))

(defmethod class-direct-superclasses ((class class))
  #+Lispworks
  (hcl:class-direct-superclasses class)
  #-Lispworks
  (clos:class-direct-superclasses class))

(defmethod class-direct-subclasses ((class built-in-class))
  #+Lispworks
  (hcl:class-direct-subclasses class)
  #-Lispworks
  (clos:class-direct-subclasses class))
