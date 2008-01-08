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

#+(and :clisp :win32)
(progn
(ffi:default-foreign-language :stdc )

(ffi:def-call-out sh-get-special-folder-location
    (:arguments (hwnd ffi:long)
		(nfolder ffi:long)
		(ppidl (ffi:c-ptr ffi:c-pointer) :out))
  (:return-type ffi:long)
  (:name "SHGetSpecialFolderLocation")
  (:library "Shell32.dll"))



(ffi:def-call-out sh-get-path-from-id-list
    (:arguments (pidl ffi:c-pointer)
                ;; FFI creates a 2048 len string automatically, returns the trunced results:
		(str (ffi:c-ptr (ffi:c-array ffi:character 2048)) :out :alloca)) 
  (:return-type ffi:long)
  (:name "SHGetPathFromIDList")
  (:library "Shell32"))

(defun get-win32-special-folder-location (n)
  (multiple-value-bind (x ptr) 
      (sh-get-special-folder-location 0 n) 
    (declare (ignorable x))
    (multiple-value-bind (retval str)
	(sh-get-path-from-id-list ptr)
      (declare (ignorable retval))
      (prog1
          (concatenate 'string
                       (string-right-trim '#.(list (code-char 0) #\/ #\\)  str)
                       "\\")
        (ffi:foreign-free ptr)))))
)
      
(defun user-documents-folder ()   
  #+(and :lispworks :win32)
  (multiple-value-bind (ret dir)  
      (win32::sh-get-folder-path 
       0 (cdr (assoc :my-documents win32::*type-csidl-pairs*)) 0 0)
    (declare (ignorable ret))
    (pathname (concatenate 'string dir "\\")))

  #+(and :clisp :win32)
  (pathname (get-win32-special-folder-location 5))
  
  #-(or (and :lispworks :win32)
        (and :clisp :win32))
  (merge-pathnames #+win32 #P"My Documents/" 
                   #-win32 #P"" 
                   (user-homedir-pathname)))

(defun make-temp-file ()
  #+:lispworks (hcl:make-temp-file)
  #+:clisp (linux:|tempnam| "tmp" "littleb")

  #+(and :win32 (not (or :lispworks :clisp)))
  (format nil "C:\\Windows\\Temp\\~A.tmp" (gensym "littleb"))
  
  #+(and :unix (not (or :lispworks :clisp)))
  (format nil "~/tmp/~A.tmp" (gensym "littleb")))

(defun prompt-for-yes-or-no (&optional (format "") &rest args)
  #+capi
  (capi:prompt-for-confirmation (apply #'format nil format args))
  #-capi
  (apply #'y-or-n-p format args))