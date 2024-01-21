#! /bin/sh
:
#ident	"@(#)smail/conf/lib:RELEASE-3_2:inst.sh,v 1.10 1996/02/26 05:16:29 woods Exp"
#
# Install a file under a list of names.
#
# usage: sh inst.sh [-lsrk] [-u user] [-g group] [-m mode] [-d dir] src [dst...]
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Note: if -d dir is specified, then the first dst should be a relative
#	pathname.

# Don't use getopt(1), as we can't rely on it being built yet, or
# existing on the system.
usage="Usage: inst [-lsrk] [-u user] [-g group] [-m mode] [-d dir] src [dst...]"
user=
group=
mode=
symlink=
strip=
rm_old=
err=0
dstpfx=
dbg=
keep_orig=

# put /etc, etc., into path for chown
PATH="$PATH:/etc:/sbin:/usr/sbin"

CHOWN="${CHOWN:-chown}"

# Note:  this shell script uses case rather than test to prevent
#	 a fork/exec on systems where test is not a builtin.

# process the list of options.
# Note:  the option letters and option arguments must be separate tokens.
while : ; do
	case $# in
	0)	break;
	esac
	case "$1" in
	--)	shift; break;;
	-[ugmd])case $# in
		1)	err=1;;
		esac
		case "$1" in
		-u)	user="$2";;
		-g)	group="$2";;
		-m)	mode="$2";;
		-d)	dstpfx="$2/";;
		esac
		shift; shift;;
	-l)	symlink=true; shift;;
	-s)	strip=true; shift;;
	-r)	rm_old=true; shift;;
	-k)	keep_orig=true; shift;;
	-D)	dbg=echo; shift;;
	-*)	err=1; break;;
	*)	break;
	esac
done

# There must be more than one operand.
case $# in
0|1)	err=1;;
esac

# If an error occured, spit out a usage message.
case "$err" in
1)	echo "$usage" 1>&2
	exit 1;;
esac

# The source file is the first operand.
srcfile="$1"; shift

# When using symlinks, copy to the first dstfile, then make all
# the others a symlink to that one.
case "$symlink" in
?*)	dstfile="$1"; shift

	dst="$dstpfx$dstfile"
	new="$dst.N"
	old="$dst.O"
	sav="$dst.SAV"

	# link the old one to a .O file
	$dbg rm -f "$old" "$new"
	$dbg ln "$dst" "$old" 2> /dev/null

	# save the original file, if desired
	case "$keep_orig" in
	?*)	if [ -f "$dst" ] && [ ! -f "$sav" ] &&
		   [ -z "`ls -l \"\$dst\" | grep ' -> '`" ]; then
			echo "save $dst as $sav"
			$dbg ln "$dst" "$sav"
		fi
		;;
	esac

	# copy to a .N and setup the ownerships and permissions
	if $dbg cp "$srcfile" "$new"; then
		:
	else
		$dbg rm -f "$old"
		echo "inst: could not copy to $new" 1>&2
		exit 1
	fi
	case "$strip" in
	?*)	strip "$new";;
	esac
	case "$user" in
	?*)	if $dbg $CHOWN "$user" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to change owner of $new" 1>&2
			exit 1
		fi;;
	esac
	case "$group" in
	?*)	if $dbg chgrp "$group" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to change group of $new" 1>&2
			exit 1
		fi;;
	esac
	case "$mode" in
	?*)	if $dbg chmod "$mode" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to change mode of $new" 1>&2
			exit 1
		fi;;
	esac

	# the < /dev/null should work like the -f flag to mv,
	# even for systems that don't have it.
	if $dbg mv "$new" "$dst" < /dev/null; then
		echo "$srcfile installed as $dst"
	else
		# some systems (HP-UX 7.0) can't do a mv if the
		# destination is an executable, however, an rm/mv
		# sequence may work
		echo "inst: try removing destination and trying again" 1>&2
		if $dbg rm -f "$dst" &&
		   $dbg mv "$new" "$dst" < /dev/null; then
			echo "inst: rm/mv succeeded: $srcfile installed as $dst" 1>&2
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to install $srcfile as $dst" 1>&2
			exit 1
		fi
	fi

	# if -r flag specified, remove the .O file
	case "$rm_old" in
	?*)	rm -f "$old";;
	esac

	# loop through the remaining operands.
	while : ; do
		case "$#" in
		0)	break;;
		esac
		case "$1" in
		-k)	keep_orig=true; shift; continue;;
		+k)	keep_orig=; shift; continue;;
		esac
		case "$1" in
		/*)	nextdst=$1; symlink=$dst;;
		*)	nextdst=$dstpfx$1; symlink=$dstfile;;
		esac
		case "$keep_orig" in
		?*)	if [ -f "$nextdst" ] && [ ! -f "$nextdst.SAV" ] &&
			   [ -z "`ls -l \"\$nextdst\" | grep ' -> '`" ]; then
				echo "save $nextdst as $nextdst.SAV"
				$dbg ln "$nextdst" "$nextdst.SAV"
			fi
			;;
		esac
		$dbg rm -f "$nextdst"
		if $dbg ln -s "$symlink" "$nextdst"; then
			echo "$symlink symlinked to $nextdst"
		else
			echo "inst: symlink failed for $nextdst"
			err=1
		fi
		shift
	done
	exit $err
	;;
esac

# when not using symbolic links, create the first dst, then try to
# make each successive dst a hard link to the previous.  If the hard
# link fails, then copy, creating a backup file as needed.
lastdst=

while : ; do
	case $# in
	0)	break;;
	esac
	case "$1" in
	-k)	keep_orig=true; shift; continue;;
	+k)	keep_orig=; shift; continue;;
	/*)	dst="$1";;
	*)	dst="$dstpfx$1";;
	esac
	shift
	new="$dst.N"
	old="$dst.O"
	sav="$dst.SAV"

	case "$keep_orig" in
	?*)	if [ -f "$dst" ] && [ ! -f "$sav" ]; then
			echo "save $dst as $sav"
			$dbg ln "$dst" "$sav"
		fi
		;;
	esac
	case "$lastdst" in
	?*)	# link to previous
		$dbg rm -f "$new" "$old"
		$dbg ln "$dst" "$old" 2> /dev/null
		if $dbg ln "$lastdst" "$new" 2> /dev/null
		then
			if $dbg mv "$new" "$dst" < /dev/null
			then
				echo "$lastdst linked to $dst"
				case "$rm_old" in
				?*)	rm -f "$old";;
				esac
				lastdst="$dst"
				continue
			else
				# some systems (HP-UX 7.0) can't do a mv
				# if the destination is an executable,
				# however, an rm/mv sequence may work
				echo "inst: try removing destination and trying again" 1>&2
				if $dbg rm -f "$dst" &&
				   $dbg mv "$new" "$dst" < /dev/null; then
					echo "inst: rm/mv succeeded: $lastdst linked to $dst" 1>&2
					lastdst="$dst"
					continue
				else
					$dbg rm -f "$new" "$old"
					echo "inst: failed to install $srcfile as $dst" 1>&2
					err=1
					continue
				fi
			fi
		fi;;
	*)	# copy
		$dbg rm -f "$new" "$old"
		$dbg ln "$dst" "$old" 2> /dev/null;;
	esac

	# copy to the current destination
	lastdst=

	if $dbg cp "$srcfile" "$new"; then
		:
	else
		echo "inst: could not copy to $new" 1>&2
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
			echo "inst: failed to change owner of $new" 1>&2
			err=1
			continue
		fi;;
	esac
	case "$group" in
	?*)	if $dbg chgrp "$group" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to change group of $new" 1>&2
			err=1
			continue
		fi;;
	esac
	case "$mode" in
	?*)	if $dbg chmod "$mode" "$new"; then
			:
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to change mode of $new" 1>&2
			err=1
			continue
		fi;;
	esac

	# the < /dev/null should work like the -f flag to mv,
	# even for systems that don't have it.
	if $dbg mv "$new" "$dst" < /dev/null; then
		echo "$srcfile installed as $dst"
	else
		# some systems (HP-UX 7.0) can't do a mv if the
		# destination is an executable, however, an rm/mv
		# sequence may work
		echo "inst: try removing destination and trying again" 1>&2
		if $dbg rm -f "$dst" &&
		   $dbg mv "$new" "$dst" < /dev/null; then
			echo "inst: rm/mv succeeded: $srcfile installed as $dst" 1>&2
		else
			$dbg rm -f "$new" "$old"
			echo "inst: failed to install $srcfile as $dst" 1>&2
			err=1
			continue
		fi
	fi
	lastdst="$dst"

	# if -r flag specified, remove the .O file
	case "$rm_old" in
	?*)	rm -f "$old";;
	esac
done

exit $err
