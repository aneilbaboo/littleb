(in-package #I@LIBRARY)

(setf  (ignore-paths nil) ; globally ignore
       '("**/cvs/" "**/CVS/") ; any folders named CVS or cvs
       (ignore-paths)
       '("*.~lisp" "*.~LISP"))

(setf (library-compile-directives) 
      '(:clear
	@LIBRARY/units/null 
	(:reload @LIBRARY/units/dimensionalization)	
	@LIBRARY/units/standard
	(:reload @LIBRARY/units/dimensionalization)	
	@LIBRARY/biochem/2-dimensional
	(:reload @LIBRARY/biochem/dimensionalization @LIBRARY/units/dimensionalization) 
	@LIBRARY/biochem/3-dimensional
	(:reload @LIBRARY/biochem/dimensionalization @LIBRARY/units/dimensionalization @LIBRARY/units/null) 
	@LIBRARY/biochem/non-dimensional 
        @LIBRARY/util
	@LIBRARY/biochem @LIBRARY/biochem/basic-ode-modelling
	@LIBRARY/biochem/multisided-cell @LIBRARY/biochem/multisided-cell/flat-lattice
        @LIBRARY/biochem/complex
	@LIBRARY/matlab/ode-translation
	@LIBRARY/numerica/ode-translation
        ;#+:lispworks @LIBRARY/gui
	;#+:lispworks @LIBRARY/gui/file-browser
	;#+:lispworks @LIBRARY/gui/new
	;#+:lispworks @LIBRARY/gui/error
	;#+:lispworks @LIBRARY/gui/main
))


