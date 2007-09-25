;;; This is the little b initialization file for the BGUI application.
;;;
;;; It is loaded when the system is initialized or reset as with (B:INIT) or (B:RESET).
;;; 
;;; A default version is provided in the little b root folder; it may be overridden by
;;; a user-specific file in the user home directory.  On Unix systems, this is usually,
;;; /home/USERNAME/littleb/.init.lisp;  On Windows systems, the user specific file is
;;; in "My Documents/littleb/.init.lisp"
;;;

;;; This is a good place to include default files or set global variables.
;;;
(in-package :b-user)

;; must set search paths where libraries can be found first:
(setf *library-search-paths* (list (get-b-path :root "libraries/")))

;; 
;; Now, include default files:
;;
(include b/math :use :verbose nil) ; load the basic math system

;; load a specific modeling regime here, if you wish; 
;; however, note that doing so may prevent loading other files with conflicting assumptions.
;;    
;; e.g., (include b-user/3d-ode-biochemistry) ; this prevents non-dimensional modeling files from loading

;; to clear the system WITHOUT loading this initialization file, call (B:INIT NIL)

;;
;; Set your matlab work path here: 
;;; E.g., on windows:
;;; {#/b/matlab/settings:*model-output-default-pathname* := #P"c:/matlab7/work/"} ; don't forget trailing /
;;;       on Linux,
;;; {#/b/matlab/settings:*model-output-default-pathname* := #P"~/matlab/work/"}
