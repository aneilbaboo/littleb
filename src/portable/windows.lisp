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


(in-package :portable)


#+:clisp
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
                ;; we'll pre-allocate a 2048 buffer -
                ;; FFI will return the truncated results:
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
      (prog1 str
        (ffi:foreign-free ptr)))))
)

(defvar *win32-folder-ids*
  '((:desktop 0)
    (:start-menu-programs 2)
    (:my-documents 5)
    (:favorites 6)
    (:start-menu-programs-startup 7)
    (:recent 8)
    (:sendto 9)
    (:start-menu 11)
    (:my-music 13)
    (:my-videos 14)
    (:desktop 16)
    (:nethood 19)
    (:fonts 20)
    (:templates 21)
    (:all-users-start-menu 22)
    (:all-users-start-menu-programs 23)
    (:all-users-start-menu-startup 24)
    (:all-users-desktop 25)
    (:appdata 26)
    (:printhood 27)
    (:local-settings-application data 28)
    (:all-users-favorites 31)
    (:local-settings-temporary-internet-files 32)
    (:cookies 33)
    (:local-settings-history 34)
    (:all-users-application-data 35)
    (:windows 36)
    (:system32 37)
    (:program-files 38)
    (:my-pictures 39)
    (:documents-and-settings 40)
    (:system32 41)
    (:program-files-common-files 43)
    (:all-users-templates 45)
    (:all-users-documents 46)
    (:all-users-start-menu-administrative tools 47)
    (:start-menu-administrative tools 48)
    (:all-users-my-music 53)
    (:all-users-my-pictures 54)
    (:all-users-my-videos 55)
    (:resources 56)
    (:cd-burning 59)
    (:temp       36 "Temp")))


(defun get-windows-pathname (folder-id &optional str &rest args)
  (let* ((assoc         (assoc folder-id *win32-folder-ids*))
         (num           (second assoc))
         (id-subfolders (cddr assoc)))
    (assert num (folder-id)
      "Invalid argument to ~S: ~S is not one of ~S"
      'get-windows-named-folder 
      folder-id
      (mapcar #'car *win32-folder-ids*))
    
    (let ((path #+:lispworks (nth-value 1 
                                        (win32::sh-get-folder-path 0 num 0 0))
                #+:clisp     (get-win32-special-folder-location num)
                #-(or :clisp :lispworks)
                (enough-namestring
                 (get-windows-pathname-unsupported 
                  (first (find num *win32-folder-ids* :key #'second))))))
      (pathname (format nil "~A\\~{~A\\~}~@[~?~]"
                        (string-right-trim '#.(list (code-char 0) #\/ #\\)
                                           path)
                        id-subfolders
                        str args)))))

(defun get-windows-pathname-unsupported (folder-id)
  (labels ((ds (subpath) (rec 
                          :documents-and-settings
                          "~A\\" (list subpath)))
           (rec (folder-id &optional str args) 
             (merge-pathnames (apply #'format nil str args)
                              (get-windows-pathname-unsupported folder-id)))
           (find-path (x)
             (loop for drive from (char-code #\C) to (char-code #\Z)
                   for path = (ignore-errors 
                                (probe-file (format nil "~A:\\~A\\" 
                                                    (code-char drive) x)))
                   when path return path)))
    (ecase  folder-id
      (:desktop (ds "Desktop"))
      (:start-menu-programs (ds "Start Menu"))
      (:my-documents (ds "My Documents"))
      (:favorites (ds "Favorites"))
      (:start-menu-programs-startup (rec :start-menu "Programs\\Startup\\"))
      (:recent (ds "Recent"))
      (:sendto (ds "SendTo"))
      (:start-menu (ds "Start Menu"))
      (:my-music (ds "My Music"))
      (:my-videos (ds "My Videos"))
      (:nethood (ds "Net Hood"))
      (:fonts (rec :windows "Fonts"))
      (:system32 (rec :windows "System32"))
      (:windows (find-path "Windows"))    
      (:program-files (find-path "Program Files"))
      (:documents-and-settings  (substitute #\\  #\/ 
                                            (format nil "~AMy Documents/"
                                                    (user-homedir-pathname))))))) 