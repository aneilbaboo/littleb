Getting up and running with the little b source code

This file explains how to get little b running in a Lisp 
implementation of your choice.  A development environment 
containing little b is also available at http://littleb.org.

I Installing the Source Tree:

	1) Checkout little b if you haven't already:

	cvs -z3 -d:pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P b1 
	=> creates the folder b1/

	2) Checkout LISA and graph-tools inside the b1 folder:
	
	   a) Switch to the b1 folder:
		   cd b1      
        
	   b) Either execute the script/batch file found in the b1 folder: 
                      get-cvs-modules.bat (on windows)
                      sh get-cvs-modules.bat (on unix)

           The cvs program will need to be in the Shell's path.

           Or, do it manually:
		cvs -z3 -d :pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa co -P lisa
        	 => creates the folder lisa/ inside b1/
		cvs -z3 -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
        	 => creates the folder graph-tools/ inside b1/

	When you're done, the b1 folder should look like this:

	b1
	  \asdf			<--- contains the ASDF defsystem 
	  \graph-tools		<--- the graph-tools library
	  \libraries		<--- little b system and examples libraries
	  \lisa			<--- the LISA reasoning system
	  \src			<--- the main little b source tree 
	  \support			<--- initialization files
	  .cvsignore
	  b1.asd			<--- the ASDF system specification file 
	  license.txt		<--- MIT license
	  littleb.lisp		<--- loads little b
	  readme.txt 		<--- this file
	
II Loading Little b:

	1. Open your Lisp implementation, at the prompt, load the littleb.lisp file,
	changing "path/to/b1/" to point to the location of your b1 folder:

      (load "path/to/b1/littleb.lisp") 
  

	2. Compile the library.  You should only need to this once, or anytime you update 
	the little b source code:

      (compile-library 'b)

III Lisp Implementations

Little b is known to run in
    * Allegro CL 8.x
    * Lispworks 4.x and 5.x
    * CLISP 2.41 and higher (buggy - produces bad math!)


IV Dependencies:

Little b depends on LISA (Lisp Intelligent Software Agents) and the Graph-tools library, both of which can be obtained from SourceForge.


V Bug Reports:

Any bug reports should be submitted to http://sourceforge.net/tracker/?atid=851502&group_id=169724

