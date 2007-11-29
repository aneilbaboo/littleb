#!/bin/bash
# $Revision: 1.2 $
set -e
trap "kill 0" 2

function usage()
{
	echo "USAGE: make-release [-euh] [-o name -t btag \\"
  echo "                            --littleb btag --littleb broot \\"
  echo "                            --lisatag ltag --lisa lroot \\"
	echo "                            -l lisp]"
	echo " -o name       output file base name (default=littleb-src-TAG)"
  echo " -e            export files (no CVS info)"
	echo " -u            update the release only (do not remove directories)"
 	echo "                 if current files were exported, no update action occurs"
	echo " -t tag        get files associated with the CVS tag (default=$littlebTag)"
  echo " --btag tag    (same as -t)"
  echo " --littleb     cvsroot to littleb repository"
	echo " --lisatag tag uses lisa referenced by this tag (default=$lisaTag)"  
	echo " --lisa        cvsroot to lisa repository"
	echo " -l lisp       Lisp interpreter which will run compile-all.lisp "    
	echo " -q            quiet (hides program output)"
	echo " -Q            really quiet (hides even messages from this script)"
	echo " -h help       this output"
}

function msg () 
{ if $showMessages; then echo "${*}";	fi; }

function errorMsg () { echo "${*}" ; }

function run ()
{ 
	msg "${*}";
	echo ${*}
	if $showProgramOutput
	then eval ${*}
	else eval ${*} >> /dev/null;
	fi
}

function checkOk ()
{
  if [ $? != 0 ]
  then
		if [ "${*}" ];	then errorMsg "FAILED: $*"  ; fi
    errorMsg "EXITING: Output files not not created." 
    exit 1
	else 
		msg SUCCESS: ${*}
  fi
}

function checkOut ()
{ local	module=$1
  local cvsroot=$2
  local tag=$3

	if ! $updateFlag && [ -d $1 ]; then delete $module; fi

	if $exportFlag
	then  if [ -d $module ]
				then	msg "${module} exists, not exporting (because -u flag is set)"
				else	msg "Exporting ${module} \("tag="$tag\) from $cvsroot"
							run	cvs -z3 -d $cvsroot export -r $tag $module
				fi
	else
				if [ -d $module ]
				then	msg "${module} exists, updating only"
							run cvs -z3 -d $cvsroot update -PR -r $tag $module
				else
							msg  Checking out ${module} \("tag="$tag\) from $cvsroot
							run cvs -z3 -q -d $cvsroot co -PR -r $tag $module
				fi
	fi
	checkOk `echo "Getting ${1} from repository"`
}		

function delete ()
{ # $1=file
	if [ -d ${1} ]
	then 	msg Deleting `realpath ${1}`
				rm -frd ${1}				
	elif [ -e ${1} ]
	then 	msg Deleting `realpath ${1}`
				run rm ${1}
	fi
}

#
# SETUP VARIABLES:
# 
littlebRoot=:pserver:anonymous@littleb.cvs.sourceforge.net:/cvsroot/littleb
lisaRoot=:pserver:anonymous@lisa.cvs.sourceforge.net:/cvsroot/lisa
lisaTag=RELEASE_3_2
littlebTag=HEAD
exportFlag=false
updateFlag=false
lisp=
showMessages=true
showProgramOutput=true

while getopts "euht:o:l:m:" optname
  do
    case 	"$optname" in
	    e)				exportFlag=true;;
			u)				updateFlag=true;;
      t|-btag)	littlebTag=${OPTARG}
                tagName=-$littlebTag
								;;
			-lisatag) lisaTag=${OPTARG};;
      o)				outputName=${OPTARG};;
			l)				lisp=${OPTARG};;
			-lisa) 		lisaRoot=${OPTARG};;
			-littleb) littlebRoot=${OPTARG};;
			h)				usage
								exit 0
								;;	
			q)				showProgramOutput=false;;
			Q)  	    showProgramOutput=false
								showMessages=false;;
      *)
        echo "Unknown error while processing options"
				exit 1
        ;;
    esac
  done

# finish setting up variables
if [ ! $outputName ]; 
then 	outputName=littleb-src${tagName}
fi

msg "Options:   exportFlag=${exportFlag}, updateFlag=${updateFlag}"
msg "           littlebTag=$littlebTag,lisaTag=$lisaTag"
msg "           littlebRoot=$littlebRoot"
msg "           lisaRoot=$lisaRoot"
msg "           lisp=$lisp"
msg This script will make $outputName.tar.gz and $outputName.zip

# DELETE OUTPUT FILES:
rm -frd output

# CHECKOUT

checkOut b1 $littlebRoot $littlebTag

run cd b1

checkOut lisa $lisaRoot $lisaTag

checkOut graph-tools  $littlebRoot $littlebTag

msg "CHANGING to library directory"

run cd libraries

checkOut segment-polarity  $littlebRoot $littlebTag

checkOut scaffold  $littlebRoot $littlebTag

checkOut multisite $littlebRoot $littlebTag

run cd ../..

#
# LISP COMPILE
#
if [ $lisp ]
then
	run 'echo "(load \"compile-all.lisp\")" | $lisp'
	checkOk "compiling code and libraries"
fi

#
# CREATE ARCHIVES
#
run mkdir output

run tar -cf ${outputName}.tar output/b1

checkOk `echo Making output/${outputName}.tar`

run gzip output/${outputName}.tar

checkOk `echo Making output/${outputName}.tar.gz`

run zip -q -r ${outputName}.zip output/b1

checkOk `echo Making output/${outputName}.zip`

echo FINISHED

