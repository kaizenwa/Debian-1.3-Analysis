#!/bin/sh
#	Turn the help text file into something that C can deal with
SRC=XCal.help
DST=xcal_help.h
( 	echo 'char helpdata[] = "\'
   	sed -e 's/$/\\n\\/' $SRC
	echo '";'
) > $DST
