echo off
echo This script checks out libraries required by little b from SourceForge.net.  To work, cvs.exe (Concurrent Version System) must be in the path.

cvs -d:pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa co -P lisa
cvs -d:pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools