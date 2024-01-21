#!/bin/sh
# @(#) batchsmtp.sh,v 1.6 1992/09/06 04:37:45 tron Exp

# deliver messages accumlated into subdirectories of the
# outq spool directory.  Subdirectory names are based on
# the actual hostnames involved:

LOCALHOST=veritas.veritas.com
MAXSIZE=100000

OUTQ=/usr/spool/smail/outq
UUX=/usr/bin/uux

cd $OUTQ || exit 1

# loop through all of the subdirectories
for host in *
do
    (
	# change to directory or exit subshell
	test -d $host || exit
	cd $host || exit 1

	# send multiple batches
	while :
	do
		# get the list of message files; quit if none
		msgs="`ls 2>/dev/null | grep '^q'`"
		test -n "$msgs" || break

		# accumulate until total size exceeds maximum
		send=
		sz=0
		for f in $msgs
		do
			send="$send $f"
			n=`wc -c <$f 2>/dev/null`
			test -n "$n" || continue
			sz=`expr $sz + $n`
			test $sz -lt $MAXSIZE || break
		done

		# send messages, adding HELO and QUIT commands
		( echo "HELO $LOCALHOST"
		  for f in $send
		  do
			  cat $f
		  done
		  echo QUIT ) | $UUX - $host!rsmtp

		# remove messages that were sent
		for f in $send
		do
			rm $f
		done
	done
    )
done

exit 0
