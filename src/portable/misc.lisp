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

(in-package portable)

(defun user-documents-folder (&optional (str "") &rest args) 
  #+:win32 (apply #'get-windows-pathname :my-documents str args)
  (user-homedir-pathname))

(defun temp-pathname (&optional base-name (n 0))
  (format nil "~A~A.tmp~A" 
          #+:unix "/tmp/"
          #+:win32 (merge-pathnames (get-windows-pathname :windows)
                                    "temp/")
          (format nil "~A~A" (or base-name "lispport")
                  #+:unix "" #-:unix n)
          #+:unix (format nil ".~A" n)
          #-:unix ""))

(defvar *browser-path* ())

(defun determine-browser-path ()
  (or 
   #+:unix #+:mac (or (probe-file "/Applications/safari")
                      (probe-file "/Applications/firefox")
                      (probe-file "/Applications/iexplore"))
           #-:mac (or (probe-file "/usr/bin/firefox")
                      (probe-file "/usr/bin/mozilla")
                      (probe-file "/usr/bin/opera"))
           #+:win32 (or (probe-file 
                         (get-windows-pathname 
                          :program-files 
                          "Internet Explorer\\iexplore.exe"))
                        (probe-file 
                         (get-windows-pathname 
                          :program-files 
                          "Mozilla Firefox\\firefox.exe")
                         (probe-file 
                          (get-windows-pathname
                           :program-files 
                           "Opera\\opera.exe"))))
           (error "Cannot determine location of web browser.  Set ~S to the location of your preferred web browser."  '*browser-path*)))
(defun browse (url)
  (run-shell-command "\"~A\" \"~A\"" (or *browser-path* 
                                 (setf *browser-path* (determine-browser-path)))
                     url))

(defvar *verbose-out* nil)
;;;;
;;;; RUN-SHELL-COMMAND: lifted from ASDF
;;;;
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *verbose-out* "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program  
      #+win32 "sh" #-win32 "/bin/sh"
      (list  "-c" command)
      #+win32 #+win32 :search t
      :input nil :output *verbose-out*))
    
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    (excl:run-shell-command command :input nil :output *verbose-out*)
    
    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)
    
    #+clisp				;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
	       (ccl:external-process-status
		(ccl:run-program "/bin/sh" (list "-c" command)
				 :input nil :output *verbose-out*
				 :wait t)))
    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)
    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    )) 

(defun prompt-for-yes-or-no (&optional (format "") &rest args)
  #+capi
  (capi:prompt-for-confirmation (apply #'format nil format args))
  #-capi
  (apply #'y-or-n-p format args))

;;;
;;; TAKEN FROM ASDF
;;;
(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *verbose-out* "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program  
      #+win32 "sh" #-win32 "/bin/sh"
      (list  "-c" command)
      #+win32 #+win32 :search t
      :input nil :output *verbose-out*))
    
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    (excl:run-shell-command command :input nil :output *verbose-out*)
    
    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)
    
    #+clisp				;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
	       (ccl:external-process-status
		(ccl:run-program "/bin/sh" (list "-c" command)
				 :input nil :output *verbose-out*
				 :wait t)))
    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)
    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))