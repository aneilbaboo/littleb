echo off
echo This script checks out libraries required by little b from SourceForge.net.  To work, cvs.exe (Concurrent Version System) must be in the path.

rmdir lisa
cvs -d :pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa export -r RELEASE_3_2 lisa

rmdir graph-tools
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
