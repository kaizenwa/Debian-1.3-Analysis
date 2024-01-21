#!/bin/sh
# @(#) nukeq.sh,v 1.1 1991/10/26 22:57:56 tron Exp
#
# Remail all of the mail queued for a particular system.  Run as
#
# nukeq <sysname>
#
# Does nothing unless the paths files have been rebuilt so that all
# final destinations in the mail messages are now reached through a
# different neighbor.  Otherwise mail will just be requeued for the
# same system again.
#
# Doesn't fix the requestor field in the new X file, so errors in mailing
# further along the line will probably be returned to the person who ran
# nukeq rather then the person who originally sent the mail.
#
# The destinations are aggressively rerouted on the assumption that the
# old path is irrelevant to the new first-hop.  This may result in mail
# sent to an incorrect system with the same name as the correct system.
#
# If uucico starts up while this thing is running, the world will come
# to an end.


if [ `id|sed 's/^uid=\([0-9]*\).*$/\1/'` -ne 0 ]
then
	echo Not superuser, no can do.
	exit
fi

if [ $# -ne 1 ]
then
	echo Need a system name
	exit
fi

sysname=$1

cd /usr/spool/uucp/$sysname

for cfile in C.*
do
	(
		echo Doing C-file $cfile

		# For each C-file
		# Grab the line describing the D file

		read type source dest sender opts data mode notify || {
			echo $cfile is empty, skipping...
			continue
		}

		if [ "$type" != "S" -o "$source" != "$data" \
		  -o "$sender" != "$notify" -o "$opts" != "-" ]
		then
			echo $cfile has invalid line 1, skipping...
			continue
		fi

		case $dest in
			D.*)	;;
			*)	echo $cfile line 1 isn\'t a Dfile, skipping...
				continue
				;;
		esac

		dfile=$source
		echo D-file is $dfile

		# Grab the line describing the X file
		read type source dest sender opts data mode notify || {
			echo $cfile missing line 2, skipping...
			continue
		}

		if [ "$type" != "S" -o "$source" != "$data" \
		  -o "$sender" != "$notify" -o "$opts" != "-" ]
		then
			echo $cfile has invalid line 2, skipping...
			continue
		fi

		case $dest in
			X.*)	;;
			*)	echo $cfile line 2 isn\'t an Xfile, skipping...
				continue
				;;
		esac

		xfile=$source
		echo X-file is $xfile

		# Make sure the X-file was the last line in the C-file
		read type
		if [ -n "$type" ]
		then
			echo $cfile has more than 2 lines, skipping...
			continue
		fi

		# Read the xfile looking for the destinations
		dests=`egrep '^C rmail ' < $xfile | sed 's/^C rmail //'`

		if [ `echo $dests | wc -w` -eq 0 ]
		then
			echo $cfile isn\'t an rmail job, skipping...
			continue
		fi

		# Build newdests, which is the same as dests for users on
		# machines other than the one being nuked, and is $sysname!user
		# for users on the same system (this will result in mail being
		# queued back onto the same system again).

		newdests=''
		for user in $dests
		do
			if [ -n "`echo $user | grep !`" ]
			then
				newdests="$newdests $user"
			else
				newdests="$newdests $sysname!$user"
			fi
		done

#		echo Rerouting to $newdests

		# Re-send the mail, using aggressive rerouting
		#smail -R -v $newdests < $dfile
		# smail3.1 doesn't have -R
		smail -v $newdests < $dfile

		# And kill off the old message
		rm $cfile $dfile $xfile
		echo Done
	) < $cfile
done
exit
