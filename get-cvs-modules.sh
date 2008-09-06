rm -r lisa
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P lisa
rm -r graph-tools
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
rmdir slime
cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime