#!/bin/bash
rm -r lisa
if [ -z $1 ]; then
   cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P lisa
else
   cvs -d :ext:${1}@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P lisa
fi
rm -r graph-tools
if [ -z $1 ]; then
   cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
else
   cvs -d :ext:${1}@littleb.cvs.sourceforge.net:/cvsroot/littleb co -P graph-tools
fi
rm -r slime
cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot export -r SLIME-2-1 slime
