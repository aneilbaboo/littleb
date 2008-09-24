@echo USAGE: get-cvs-modules.bat [name]
@echo Where name = SourceForge login id for the littleb project.
@echo              If name is missing, checkout is anonymous 
@echo This script checks out libraries required by little b from SourceForge.net.  
@echo To work, cvs.exe (Concurrent Version System) must be in the path.
@echo And CVS_RSH=path/to/plink.exe

rmdir /s /q lisa
rmdir /s /q graph-tools
rmdir /s /q slime

@if (%1)==() goto anonymous_checkout
@set protocol=:ext:%1
@goto do_checkout
@:anonymous_checkout
@set protocol=:pserver:anonymous

@:do_checkout
cvs -z3 -d %protocol%@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P lisa
cvs -z3 -d %protocol%@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
cvs -z3 -d :pserver:anonymous@common-lisp.net:/project/slime/cvsroot co slime

@echo Checked out lisa and graph-tools modules as %1 from littleb on Sourceforge
@echo Checked out slime as anonymous from common-lisp.net


