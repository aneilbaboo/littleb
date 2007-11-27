#+:lispworks (load-all-patches)
(in-package :cl-user)

#-:asdf+ (load "../asdf/asdf+.lisp")

(asdf:delete-binaries :b1)
(asdf:delete-binaries :lisa)
(asdf:delete-binaries :graph-tools)

;; load, compiling all systems:
(load "b1/littleb.lisp")

(in-package :b-user)

(b:compile-library 'b)

(in-package :cl-user)

(quit)


