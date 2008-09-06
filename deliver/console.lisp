(defpackage :b-console
  (:use :b :cl)
  (:export #:run-b-top-level #:*littleb-id-string* #:*version* #:*help-text* #:*license*))

(in-package :b-console)

(defvar *license*)
(defvar *help-text*)
(defvar *version*)
(defvar *littleb-id-string*)

(defun run-b-top-level ()
  (setf b-system:*b-root-directory* (make-pathname :name nil :type nil :version nil
                                                   :defaults (probe-file (first system:*line-arguments-list*)))
        b:*library-search-paths* (list (merge-pathnames "libraries/" b-system:*b-root-directory*)))
  (princ *version*)
  (in-package :b-user)
  (process-commands))

(defun cmd-signal-p (x)
  (and (stringp x)
       (> (length x) 0)
       (char= (char x 0) #\-)))

(defun read-command-line ()
  (let ((cmds (copy-list (rest system:*line-arguments-list*))))
    (flet ((get-data (signal &key (min 1) (max nil))
             (loop for next = (first cmds)
                   while (and next (not (cmd-signal-p next)))
                   collect (pop next) into data
                   finally (if (or (and min (> min (length data)))
                                   (and max (< max (length data))))
                               (console-error "Invalid number of arguments for command line parameter ~A" signal)
                             (return data)))))
      (loop for signal = (pop cmds)
            while signal
            if (cmd-signal-p signal)
            append (mutils:test (#'string= signal)
                     (("-h" "--help")         `((:help . t) (:quit . t)))
                     (("-l" "--load")         `((:load ,@(get-data signal :min 1))))
                     (("-i" "--include")      `((:include ,@(get-data signal :min 1))))
                     (("-t" "--listen")       `((:listen . ,(or (get-data signal :min 0 :max 1)
                                                                swank::default-server-port))))
                     ("-p"                    `((:add-libraries ,@(get-data signal :min 1))))
                     ("-P"                    `((:set-libraries ,@(get-data signal :min 1))))
                     ("--init"                `((:init-file ,@(get-data signal :min 1 :max 1))))
                     ("--license"             `((:license . t) (:quit . t)))
                     ("--version"             `((:version .t) (:quit . t)))
                     (("-q" "--quit")         `((:quit . t)))
                     (t                       (console-error "Invalid command line argument ~A." signal)))
            into cmd-pairs
            finally (let ((ht (make-hash-table)))
                      (dolist (pair cmd-pairs)
                        (setf (gethash (car pair) ht) (cdr pair)))
                      (return ht))))))
         
(defun console-error (str &rest args)
  (format t "Error: ~?~%~
             littleb --help for a list of command line arguments~%" str args)
  (quit))
              
(defun process-commands ()
  (let ((cmds (read-command-line)))
    (flet ((cmd (signal) (gethash signal cmds)))
      (when (cmd :help)          (format t *help-text*))
      (when (cmd :license)       (princ *license*))
      (when (or (cmd :help) (cmd :version) (cmd :license)) (lispworks:quit))
      (when (cmd :init-file)     (unless (probe-file (cmd :init-file))
                                   (console-error "Init-file ~A doesn't exist" (cmd :init-file)))
                                 (setf b-system:*b-init-file* (probe-file (cmd :init-file))))
      (when (cmd :set-libraries) (setf *library-search-paths* (cmd :set-libraries)))
      (when (cmd :add-libraries) (setf *library-search-paths* (append *library-search-paths*
                                                                      (cmd :add-libraries))))
      (init)
      (when (cmd :load)          (dolist (file (cmd :load))
                                   (load file)))
      (when (cmd :include)       (dolist (ipath (cmd :include))
                                   (include-dynamic ipath)))
      (when (cmd :listen)        (swank:create-server (cmd :listen)))
      (when (cmd :quit)          (lispworks:quit))
      (lispworks:start-tty-listener nil))))

(setf swank:*log-events* t)

(setf *littleb-id-string* (multiple-value-bind (major minor revision) (b:littleb-version)
                            (format nil "little b Version ~A.~A.~A" major minor revision))
      *version* (multiple-value-bind (second minute hour date month year)
                    (get-decoded-time)
                  (format nil "~A~%~
                            Copyright (C) 2005-8, Aneil Mallavarapu~%~
                            littleb --license for license info~%~
                            littleb --help for other command line parameters~%~
                            built ~A-~A-~A ~2D:~2D:~2D~%~
                            http://www.littleb.org~%~%"
                            *littleb-id-string*
                            month date year hour minute second)))
(setf *help-text*
"usage: littleb [options]
options:
-l file*         load file(s)
--load file*
-i file*         load file relative to library path: ie, (include file)
--include file*
-p directory*    add little b library paths
-P directory*    set little b library paths (overrides existing library paths)
-init file       use initialization file
-t [port]        listen on TCP/IP port (default=4005) using swank
--listen [port]
-q, --quit       force quit after processing commands
-h, --help       this help information
--license        prints the little b license
")

(setf *license* 
"The MIT License

Copyright (c) 2003-2008 Aneil Mallavarapu

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
")
