rmdir /S /Q b1
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb checkout -P b1
cd b1
rmdir /S /Q release
cvs -d :pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa checkout -Pd lisa
cvs -d :pserver:anonymous@graph-tools.cvs.sourceforge.net:/cvsroot/graph-tools checkout -dP graph-tools
cd libraries
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb checkout segment-polarity
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb checkout scaffold
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb checkout multisite
cd ../
