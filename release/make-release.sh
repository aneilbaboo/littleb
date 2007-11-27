#!/bin/bash
trap "kill 0" 2

function usage()
{
	echo "USAGE: make-release -e -t cvsTag -o name -l lisp"
  echo " -e         export files (without CVS info)"
	echo " -u         update the release only (do not remove directories)"
 	echo "              if current files were exported, no update action occurs"
	echo " -t tag     get files associated with the CVS tag (default=HEAD)"
	echo " -l lisp    path to the Lisp interpreter which will run compile-all.lisp "    
	echo " -o name    output file base name (default=littleb-src-TAG)"
	echo " -h help    this output"
}

function checkOk ()
{
  if [ $? != 0 ]
  then
		if [ $* ];	then echo FAILED: $*; fi
    echo "EXITING: Output files not not created.  Check make-release.log for details."
    exit 1
	else 
		echo SUCCESS: $*
  fi
}

function checkOut ()
{ # $1=module, $2=cvsroot, $3=exportFlag, $4=tag

	if ! $updateFlag && [ -d $1 ]; then delete $1; fi

	if [ $3 ]
	then  if -d $1
				then	echo "${1} exists, not exporting (because -u flag is set)"
				else	echo Exporting ${1} \("tag="$tag\) from $2 
							cvs -q -z3 -d $2 export -r $tag $1 >> ./make-release.log
				fi
	else
				if -d $1
				then	echo "${1} exists, updating only"
							cvs update  
						echo  Checking out ${1} \("tag="$Tag\) from $2 
				cvs -q -d $2 co -PR -r $Tag $1 >> ./make-release.log
	fi
	checkOk `echo "Getting ${1} from repository"`
}		

function delete ()
{ # $1=file
	if [ -d ${1} ]
	then 	echo Deleting `realpath ${1}`
				rm -frd ${1}				
	elif [ -e ${1} ]
	then 	echo Deleting `realpath ${1}`
				rm ${1}
	fi
}

#
# SETUP VARIABLES:
# 
while getopts "ht:o:l:m:" optname
  do
    case "$optname" in
	    e)  exportFlag=1;;
			u)	updateFlag=1;;
      t)	tag=${OPTARG};;
      o)	outputName=${OPTARG};;
			l)	lisp=${OPTARG};;
			h)	usage
						exit 0
						;;	
      *)
      # Should not occur
        echo "Unknown error while processing options"
        ;;
    esac
  done
# finish setting up variables
if [ ! outputName ]; 
then outputName=`echo littleb-src-$tag | tr "[:upper:]" "[:lower:]"`
fi

echo This script will make $outputName.tar.gz and $outputName.zip

# DELETE OUTPUT FILES:
delete make-release.log
if [ ! update ]
then	delete b1
			delete lisa
fi
delete ${outputName}.tar 
delete ${outputName}.tar.gz 
delete ${outputName}.zip

checkOut b1 :pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb

# anonymous checkout from cvs
checkOut lisa :pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa HEAD

# did this when I was having so many anonymous cvs checkout problems!
# cp -R lisa b1

cd b1

checkOut graph-tools :pserver:anonymous@graph-tools.cvs.sourceforge.net:/cvsroot/graph-tools

cd libraries

checkOk "changing to library directory"

checkOut segment-polarity :ext:am116@orchestra.med.harvard.edu:/cvs/littleb 

checkOut scaffold :ext:am116@orchestra.med.harvard.edu:/cvs/littleb 

checkOut multisite :ext:am116@orchestra.med.harvard.edu:/cvs/littleb

cd ../..

# compile using the lisp interpreter
if [ $lisp ]
then
	$lisp < compile-all.lisp
	checkOk "compiling code and libraries"
fi

tar -cf ${outputName}.tar b1

checkOk `echo Making ${outputName}.tar`

gzip ${outputName}.tar

checkOk `echo Making ${outputName}.tar.gz`

zip -q -r ${outputName}.zip b1

checkOk `echo Making ${outputName}.zip`

echo FINISHED$?
