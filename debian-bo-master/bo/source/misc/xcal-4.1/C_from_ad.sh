#!/bin/sh
#	Turn the help text file into something that C can deal with
SRC=XCal.ad
DST=xcal_ad.h
( 	echo 'static String fallbackResources[] = {'
   	sed -e '
	/^!/d
	s/^/"/
	s/$/",/' $SRC
	echo 'NULL,
};'
) > $DST
