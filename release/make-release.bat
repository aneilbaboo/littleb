set tag=%1
if (%1)==() set tag=HEAD
rmdir /S /Q b1
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb export -r %tag% b1 
cd b1
rmdir /S /Q release
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb export -r %tag% lisa
cvs -d :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb export -r %tag% graph-tools
cd libraries
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb export -r %tag% segment-polarity
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb export -r %tag% scaffold
cvs -d :ext:am116@orchestra.med.harvard.edu:/cvs/littleb export -r %tag% multisite
cd ../..
pause
