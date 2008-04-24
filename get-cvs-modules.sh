rm -r lisa
cvs -d :pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa export -r RELEASE_3_2 lisa
rm -r graph-tools
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
cp support/lisa3_2-inter-pattern-filter.patch lisa
cd lisa
patch -p0 < lisa3_2-inter-pattern-filter.patch
cd .
