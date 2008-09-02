echo off
echo This script checks out libraries required by little b from SourceForge.net.  To work, cvs.exe (Concurrent Version System) must be in the path.

rmdir lisa
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb export -r lisa

rmdir graph-tools
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools

rmdir slime
cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
