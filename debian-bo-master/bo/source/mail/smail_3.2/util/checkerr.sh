#!/bin/sh
# @(#) checkerr.sh,v 1.6 1992/09/20 12:55:08 tron Exp
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Check for errors which have been deposited in the smail error directory.
# If any new messages are found in this directory, save information
# related to those errors in the file .checkerror which is then mailed
# to the postmaster.  If the mail cannot be sent to the postmaster now,
# such as due to a configuration error, then try to send it in a future
# invocation of this script.

PATH="X_SECURE_PATH_X"; export PATH
SMAIL_PROGRAM="X_SMAIL_NAME_X"
SPOOL_DIRS="`$SMAIL_PROGRAM -bP spool_dirs`"
HOSTNAME="`$SMAIL_PROGRAM -bP primary_name`"
LOGFILE="`$SMAIL_PROGRAM -bP logfile`"
OLD_LOGFILE="`echo $LOGFILE | sed -e 's,^\(.*\)/\([^/][^/]*\)$,\1/OLD/\2,'`"
PANICLOG="`$SMAIL_PROGRAM -bP paniclog`"
OLD_PANICLOG="`echo $PANICLOG | sed -e 's,^\(.*\)/\([^/][^/]*\)$,\1/OLD/\2,'`"
Z_DOT="X_DOT_Z_X"
ZCAT="X_ZCAT_X"
PROG=$0

# go into each spooling directory
(IFS=:; for i in $SPOOL_DIRS; do echo $i; done) |
while read SPOOLDIR; do
    cd $SPOOLDIR
    if [ $? != 0 ]; then
	# spool directory did not exist, ignore it
	continue
    fi

    # cleanup old msg.* files in the input directory
    if [ -d input ]; then
	find input -name 'msg.*' -mtime +2 -print | while read fn
	do
	    rm -f "$fn"
	done
    fi

    # cleanup msglog files with no corresponding input or error file:
    if [ -d msglog ]; then (
    	cd msglog
	x="`echo [0-9]*`"
	sleep 1			# avoids an almost impossible race condition
	if [ "$x" != "[0-9]*" ]; then
	    for i in $x; do
		if [ ! -f ../input/$i ] && [ ! -f ../error/$i ]; then
		    rm -f $i
		fi
	    done
	fi
    ); fi

    # if the last run found some errors, but couldn't deliver, try again now
    if [ -s .checkerror ]; then
	# don't send to the Postmaster if configuration errors still exist
	x="`$SMAIL_PROGRAM -bv Postmaster 2>/dev/null`"
	if [ "$x" ]; then
	    $SMAIL_PROGRAM -f"<+>" -eq -m Postmaster <<EOF
From: MAILER-DAEMON
Subject: Mail errors on the host $HOSTNAME

Smail has detected new errors requiring your attention on the
host $HOSTNAME.  Messages which failed can be found in the
directory $SPOOLDIR/error and should be moved back to
$SPOOLDIR when the situation which caused the error has
been taken care of.

A sumary of these errors follows:
`cat .checkerror`
EOF
	    if [ $? -ne 0 ]; then
		continue
	    fi
	    rm -f .checkerror
	else
	    # if we cannot reach the postmaster, don't go to the next step
	    continue
	fi
    fi

    if [ ! -d error ]; then
	# no error directory, so there could not be new errors
	continue
    fi

    # find any new errors
    if [ -f .lasttimedone ]; then
	mv -f .lasttimedone .thistime
	: > .lasttimedone
	cd error; find * -newer ../.thistime -name '[0-9]*' -print
    else
	: > .lasttimedone
	cd error; find * -name '[0-9]*' -print
    fi 2> /dev/null | while read f; do
	echo ""; echo "------------------ Message $f ------------------"
	if [ -s msglog/$f ]; then
	    echo "The per-message log file contains:"
	    sed 's/^/ /' < msglog/$f
	else
	    echo "No per-message log file was created"
	fi
	echo ""; echo "Logfile entries related to this message are:"
	(if [ -f $LOGFILE ]; then
		cat $LOGFILE;
	 fi
	 if [ -f $OLD_LOGFILE.0 ]; then
		cat $OLD_LOGFILE.0;
	 elif [ -f $OLD_LOGFILE.0$Z_DOT ]; then
		$ZCAT $OLD_LOGFILE.0$Z_DOT;
	 fi
	 if [ -f $OLD_LOGFILE.1 ]; then
		cat $OLD_LOGFILE.1;
	 elif [ -f $OLD_LOGFILE.1$Z_DOT ]; then
		$ZCAT $OLD_LOGFILE.1$Z_DOT;
	 fi) | sed ":l1
		    /^[^|].*\[m$f\]/{
			:l2
			s/^/  /
			n
			/^|/!b l1
			s/^|	/|       /
			b l2
		    }
		    d"
	echo ""; echo "Paniclog entries related to this messsage are:"
	(if [ -f $PANICLOG ]; then
		cat $PANICLOG;
	 fi
	 if [ -f $OLD_PANICLOG.0 ]; then
		cat $OLD_PANICLOG.0;
	 elif [ -f $OLD_PANICLOG.0$Z_DOT ]; then
		$ZCAT $OLD_PANICLOG.0$Z_DOT;
	 fi
	 if [ -f $OLD_PANICLOG.1 ]; then
		cat $OLD_PANICLOG.1;
	 elif [ -f $OLD_PANICLOG.1$Z_DOT ]; then
		$ZCAT $OLD_PANICLOG.1$Z_DOT;
	 fi) |	grep "\[m$f\]" | sed 's/^/  /'
    done > .newerrors

    mv .newerrors .checkerror

    if [ -s .checkerror ]; then
	# don't send to the Postmaster if configuration errors still exist
	x="`$SMAIL_PROGRAM -bv Postmaster 2>/dev/null`"
	if [ "$x" ]; then
	    $SMAIL_PROGRAM -f"<+>" -eq -m Postmaster <<EOF
From: MAILER-DAEMON
Subject: Mail errors on the host $HOSTNAME

Smail has detected new errors requiring your attention on the
host $HOSTNAME.  Messages which failed can be found in the
directory $SPOOLDIR/error and should be moved back to
$SPOOLDIR when the situation which caused the error has
been taken care of.

A sumary of these errors follows:

`cat .checkerror`
EOF
	    if [ $? -eq 0 ]; then
		rm -f .checkerror
	    fi
	fi
    fi
done

exit 0
