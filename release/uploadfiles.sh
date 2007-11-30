#!/bin/bash
# Uploads all files in the relative sub-directory output 
# to upload.sourceforge.net/incoming
# $Revision: 1.2 $
set -e
trap "kill 0" 2

uploadFile () 
{
	echo Uploading $1
	echo FTP starting...
	ftp -n upload.sourceforge.net	<<FTP_COMMANDS
		quote USER anonymous
		quote PASS xyzzy@gmail.com
		cd incoming
		binary
		prompt
		mput ${1}
		bye
FTP_COMMANDS
	echo FTP finished.
}

cd output
for FILE in *
do
	uploadFile $FILE
done
cd ..
