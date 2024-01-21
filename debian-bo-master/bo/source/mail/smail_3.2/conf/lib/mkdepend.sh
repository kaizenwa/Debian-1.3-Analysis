#! /bin/sh
:
# @(#) mkdepend.sh,v 1.11 1995/07/06 17:01:23 nm4 Exp
# build a list of dependencies and insert them at the end of the make file
#
# usage: makedepend -Idir ... makefile file.c ...
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# gather together the -Idir arguments to be passed to cc
CPPFLAGS=
while :; do
    case $1 in
    -I* | -D* | -U*) CPPFLAGS="$CPPFLAGS '$1'"; shift;;
    -systype)	shift; CPPFLAGS="$CPPFLAGS -systype '$1'"; shift;;
    -?*)	shift;;
    --)		break;;
    *)		break;;
    esac
done

# get the name of the makefile from the first arg and drop it
makefile="$1"
shift

# read relevant configuration parameters
CC="`echo X_CC_X | sed -f defs.sed`"
[ -z "$CC" ] && CC=cc

# remove any previous attempts at building the new makefile
/bin/rm -f X$makefile

# remove everything in the makefile after the magic line
# and append the dependency information
( sed '/^# DO NOT REMOVE THIS LINE/q' < $makefile;
  echo '# Miscellaneous dependencies'

  # build dependencies for all files in the arg list
  for i in "$@"; do
	# ignore empty arguments
	if [ ! "$i" ]; then
		continue
	fi

	# the following line grabs all of the included files
	echo "	$CC -E -DDEPEND $CPPFLAGS $i" 1>&3
	eval $CC -E -DDEPEND $CPPFLAGS "$i" |
		# grab filenames from /lib/cpp line and filename information
		# this is of the form:
		#
		#	# line-number "filename"
		#
		# the compiler with the sun 3 produces extra garbage after
		# the quoted filename.
		sed -n 's/^#[ 	]*[0-9]*[ 	]*"\(.*\)"[ 0-9]*$/\1/p' |
		sed 's%^\./%%' |	# remove ./ prefixes
		# next remove 
		sort | uniq |
		# use awk to put a reasonable number of them on a line
		awk '
		    BEGIN {
			srcfile="'"$i"'"
			objfile=substr(srcfile, 1, length(srcfile)-2) ".o"
			line=objfile ": "
			n = 0
		    }
		    { if (length(line) + length($0) > 78 && n > 0) {
			 print line ""; line=objfile ": "; n = 0;
		      }
		      line=line " " $0; n++;
		    }
		    END { if (n > 0) { print line } }'
  done ) 3>&1 > X$makefile

# save a backup copy and replace the old makefile with the new one
mv -f $makefile .$makefile
mv X$makefile $makefile

# all done
exit 0
