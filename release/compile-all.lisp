#+:lispworks (load-all-patches)
(in-package :cl-user)

#-:asdf+ (load "../asdf/asdf+.lisp")
(push (make-pathname :name nil :type nil 
                     :defaults 
										(or *load-truename* *compile-truename*))
			asdf:*central-registry*)

(asdf:delete-binaries :b1)
(asdf:delete-binaries :lisa)
(asdf:delete-binaries :graph-tools)

;; load, compiling all systems:
(load "b1/littleb.lisp")

(in-package :b-user)

(b:compile-library 'b)

(in-package :cl-user)

(quit)


