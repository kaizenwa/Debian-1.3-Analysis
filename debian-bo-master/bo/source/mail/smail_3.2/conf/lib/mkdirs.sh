#! /bin/sh
:
#ident	"@(#)smail/conf/lib:RELEASE-3_2:mkdirs.sh,v 1.6 1996/02/26 05:18:24 woods Exp"
#
# Create directories, if they do not already exist.
#
# usage: sh mkdirs.sh [-u user] [-g group] [-m mode] dir ...
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Don't use getopt(1), as we can't rely on it being built yet, or
# existing on the system.
usage="Usage: mkdirs.sh [-u user] [-g group] [-m mode] dir ..."
user=
group=
mode=
err=0
dbg=

# put /etc, etc., into path for chown
PATH="$PATH:/etc:/sbin:/usr/sbin"

CHOWN="${CHOWN:-chown}"

# Note:  this shell script uses case rather than test, where possible
#	 to prevent a fork/exec on systems where test is not a builtin.

# process the list of options.
# Note:  the option letters and option arguments must be separate tokens.
while : ; do
	case $# in
	0)	break;
	esac
	case "$1" in
	--)	shift; break;;
	-[ugm])case $# in
		1)	err=1;;
		esac
		case "$1" in
		-u)	user="$2";;
		-g)	group="$2";;
		-m)	mode="$2";;
		esac
		shift; shift;;
	-D)	dbg=echo; shift;;
	-*)	err=1; break;;
	*)	break;
	esac
done

# There must be some operands
case $# in
0)	err=1;;
esac

# If an error occured, spit out a usage message.
case "$err" in
1)	echo "$usage" 1>&2
	exit 1;;
esac

for dir in ${1+"$@"} ; do
	# the following loop borrowed from GNU mkinstalldirs
	# Author: Noah Friedman <friedman@prep.ai.mit.edu>
	# Created: 1993-05-16
	# Public domain
	set -- fnord `echo ":$dir" | sed -ne 's/^:\//#/;s/^://;s/\// /g;s/^#/\//;p'`
	shift
	errstatus=0
	pathcomp=""
	for d in ${1+"$@"} ; do
		pathcomp="$pathcomp$d"
		case "$pathcomp" in
		-* )
			pathcomp=./$pathcomp
			;;
		esac

		if [ ! -d "$pathcomp" ]; then
			echo "mkdir $pathcomp" 1>&2
			mkdir "$pathcomp" || errstatus=$?
		fi

		pathcomp="$pathcomp/"
	done
	if [ $errstatus -eq 0 ]; then
		case "$user" in
		?*)
			if $dbg $CHOWN "$user" "$dir"; then
				:
			else
				echo "mkdirs: failed to change owner of $dir" 1>&2
			fi
			;;
		esac
		case "$group" in
		?*)
			if $dbg chgrp "$group" "$dir"; then
				:
			else
				echo "mkdirs: failed to change group of $dir" 1>&2
			fi
			;;
		esac
		case "$mode" in
		?*)
			if $dbg chmod "$mode" "$dir"; then
				:
			else
				echo "mkdirs: failed to change mode of $dir" 1>&2
			fi
			;;
		esac
	fi
	if [ ! -d "$dir" ]; then
		# if the directory doesn't already exist, complain
		echo "mkdirs: failed to create directory $dir" 1>&2
		exit 1
	fi
done

exit 0
