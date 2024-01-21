#! /bin/sh
:
#ident	"@(#)smail/conf/lib:RELEASE-3_2:instm.sh,v 1.10 1996/02/26 05:16:57 woods Exp"
#
# Install a set of files into a directory
#
# usage: sh instm.sh [-srk] [-u user] [-g group] [-m mode] dir file...
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Don't use getopt(1), as we can't rely on it being built yet, or
# existing on the system.
usage="Usage: instm.sh [-srk] [-u user] [-g group] [-m mode] dir file..."
user=
group=
mode=
err=0
strip=
rm_old=
dbg=
keep_orig=

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
	-s)	strip=true; shift;;
	-r)	rm_old=true; shift;;
	-k)	keep_orig=true; shift;;
	-D)	dbg=echo; shift;;
	-*)	err=1; break;;
	*)	break;
	esac
done

# There must be at least two operands
case $# in
0|1)	err=1;;
esac

# If an error occured, spit out a usage message.
case "$err" in
1)	echo "$usage" 1>&2
	exit 1;;
esac

# the directory name is the first operand
dir="$1"; shift

for file
do
	dst="$dir/$file"
	new="$dst.N"
	old="$dst.O"
	sav="$dst.SAV"
	$dbg rm -f "$new" "$old"
	$dbg ln "$dst" "$old" 2> /dev/null
	case "$keep_orig" in
	?*)	if [ -f "$dst" ] && [ ! -f "$sav" ]; then
			echo "save $dst as $sav"
			$dbg ln "$dst" "$sav"
		fi
		;;
	esac
	if $dbg cp "$file" "$new"; then
		:
	else
		$dbg rm -f "$old"
		echo "instm: failed to copy $file to $dst" >&2
		err=1
		continue
	fi
	case "$strip" in
	?*)	strip "$new"
		if [ -x /usr/bin/mcs ]; then
			mcs -d "$new"
		fi
		;;
	esac
	case "$user" in
	?*)	if $dbg $CHOWN "$user" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "instm: failed to change owner of $dir" 1>&2
			err=1
			continue
		fi;;
	esac
	case "$group" in
	?*)	if $dbg chgrp "$group" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "instm: failed to change group of $dir" 1>&2
			err=1
			continue
		fi;;
	esac
	case "$mode" in
	?*)	if $dbg chmod "$mode" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "instm: failed to change mode of $dir" 1>&2
			err=1
			continue
		fi;;
	esac
	if $dbg mv "$new" "$dst" < /dev/null; then
		echo "installed $file as $dst"
	else
		# some systems (HP-UX 7.0) can't do a mv if the
		# destination is an executable, however, an rm/mv
		# sequence may work
		echo "inst: removing destination and trying again" 1>&2
		if $dbg rm -f "$dst" &&
		   $dbg mv "$new" "$dst" < /dev/null; then
			echo "inst: rm/mv succeeded: $file installed as $dst" 1>&2
		else
			$dbg rm -f "$new" "$old"
			echo "instm: failed to install $file as $dst" 1>&2
			err=1
			continue
		fi
	fi
	case "$rm_old" in
	?*)	rm -f "$old";;
	esac
done

exit $err
