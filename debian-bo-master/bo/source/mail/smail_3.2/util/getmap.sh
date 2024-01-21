#! /bin/sh
# @(#) getmap.sh,v 1.6 1992/07/11 11:39:55 tron Exp
#
#    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992 Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# getmap - unshar usenet map articles into the UNSHAR_MAP_DIR directory
#
# usage: getmap [-m mapdir] [-w workdir] [-b batchfile] [-u username]
#
#	 -m mapdir	- 'mapdir' is where maps are unpacked,
#			  by default 'mapdir' is $UNSHAR_MAP_DIR
#	 -w workdir	- where logs and default batch files are kept,
#			  by default 'workdir' is 'mapdir'/work
# 	 -b batch	- 'batch' is a file of filenames to unshar,
#			  '-' impiles read filenames from stdin,
#			  by default 'batch' is 'workdir'/batch
#	 -u username	- errors are Emailed to 'username' rather than
#			  Posmaster, the username '-' imples that
#			  the errors should be written to standard error
#	 -n newsgroups	- allowed newsgroups for articles
#
# unsharmap errors will be emailed to the postmaster.
#
# default files:
#    $UNSHAR_MAP_DIR			- maps are unpacked here
#    $UNSHAR_MAP_DIR/work/getmap.log	- log of getmap activity and errors
#    $UNSHAR_MAP_DIR/work/getmap.err	- like getmap.log + skipped lines
#					  removed if no major unshar errors
#    $UNSHAR_MAP_DIR/work/batch		- default list of artciles to unshar
#    batch.work				- tmp copy of batch work, normally
#					  $UNSHAR_MAP_DIR/work/batch.work,
#					  ignored if "-w -"

# locations and constants
#
PATH="X_UTIL_PATH_X:X_SECURE_PATH_X"; export PATH
UTIL_BIN="X_UTIL_BIN_DIR_X"
UNSHAR_MAP_DIR="X_UNSHAR_MAP_DIR_X"
NEWSSPOOL="X_NEWS_SPOOL_DIR_X"
UNSHAR=$UTIL_BIN/unsharmap
GETOPT=$UTIL_BIN/getopt
AWKFILE=$UTIL_BIN/getmap.awk
GLEEM=$UTIL_BIN/gleem

# set defaults value for location that can change by option
#
MAPDIR=$UNSHAR_MAP_DIR
WORKDIR=$MAPDIR/work
BATCH=$WORKDIR/batch
USERNAME=Postmaster
NEWSGROUPS=comp.mail.maps

# parse args
#
PROG=$0
USAGE="usage: $PROG [-m mapdir] [-w workdir] [-b batch] [-u username]"
set -- `$GETOPT -n $PROG -q m:w:b:u:n:s: $*`
if [ $? != 0 ]; then
    echo $USAGE 1>&2
    exit 1
fi
for i in $*; do
    case $i in
    -m) MAPDIR=$2; shift 2;;
    -w) WORKDIR=$2; shift 2;;
    -b) BATCH=$2; shift 2;;
    -u) USERNAME=$2; shift 2;;
    -n) NEWSGROUPS=$2; shift 2;;
    -s) NEWSSPOOL=$2; shift 2;;
    --) shift; break;;
    esac
done
if [ "$#" -ne 0 ]; then
    echo $USAGE 1>&2
    exit 2
fi
if [ "$BATCH" = "-" ]; then	# catch stdin case
    BATCH=
fi
if [ "$USERNAME" = "-" ]; then	# catch the cat errors to stderr case
    REPORT="cat 1>&2"
else
    REPORT="X_SMAIL_NAME_X -ep -i -t"
fi

# set locations now that we have the flags
#
LOG=$WORKDIR/getmap.log
ERROR_LOG=$WORKDIR/getmap.err
REBUILD=$WORKDIR/getmap.rebuild

# be sure the working file does not exist, unless we read from stdin
#
if [ ! -z "$BATCH" ]; then
    BATCH_TMP="$BATCH".work
    if [ -f "$BATCH_TMP" ]; then
        echo "$PROG: working batch file $BATCH_TMP exists" 1>&2
        echo "$PROG: remove $BATCH_TMP by hand if stale" 1>&2
        exit 3
    fi
fi

# setup log files
#
if [ ! -z "$BATCH" ]; then
    BATCH_MSG="$PROG: starting work on $BATCH at `date`"
else
    BATCH_MSG="$PROG: starting work on [stdin] at `date`"
fi
echo "$BATCH_MSG" > $ERROR_LOG
if [ "$?" -ne 0 ]; then
    echo "$PROG: can not clear $ERROR_LOG" 1>&2
    exit 4
fi
touch $LOG 2>/dev/null
if [ "$?" -ne 0 ]; then
    echo "$PROG: can not write to $LOG" 1>&2
    echo "$PROG: can not write to $LOG" >> $ERROR_LOG
    (if [ "$USERNAME" != "-" ]; then
	echo "To: $USERNAME"
	echo "Subject: getmap error"
	echo ""
	echo ""
     fi
     echo "getmap error log follows -----"
     cat $ERROR_LOG
     echo "end of getmap error log =====") | eval "$REPORT"
    exit 5
fi


# save the batch file, if not from stdin
#
if [ ! -z "$BATCH" ]; then
    # do nothing if no batch file or no work in the batch file
    if [ ! -f "$BATCH" -o ! -s "$BATCH" ]; then
	echo "$PROG: no work in $BATCH" >> $LOG
	rm -f $ERROR_LOG
        exit 0
    fi
    mv $BATCH $BATCH_TMP
    if [ "$?" -ne 0 ]; then
        echo "$PROG: could not move $BATCH to $BATCH_TMP" 1>&2
        echo "$PROG: could not move $BATCH to $BATCH_TMP" >> $ERROR_LOG
	(if [ "$USERNAME" != "-" ]; then
	    echo "To: $USERNAME"
	    echo "Subject: getmap error"
	    echo ""
	    echo ""
	 fi
	 echo "getmap error log follows -----"
	 cat $ERROR_LOG
	 echo "end of getmap error log =====") | eval "$REPORT"
	cat $ERROR_LOG >> $LOG
	exit 6
    fi

# if work from stdin, prep a file to be used to save a copy
#
else
    BATCH_TMP=$WORKDIR/getmap.in$$
    cat /dev/null > $BATCH_TMP
    if [ "$?" -ne 0 ]; then
        echo "$PROG: could not clear $BATCH_TMP to hold a copy of [stdin]" 1>&2
        echo "$PROG: could not clear $BATCH_TMP to hold a copy of [stdin]" >> $ERROR_LOG
	(if [ "$USERNAME" != "-" ]; then
	    echo "To: $USERNAME"
	    echo "Subject: getmap error"
	    echo ""
	    echo ""
	 fi
	 echo "getmap error log follows -----"
	 cat $ERROR_LOG
	 echo "end of getmap error log =====") | eval "$REPORT"
	cat $ERROR_LOG >> $LOG
	exit 7
    fi
fi

# process the map artcile files
#
if [ ! -z "$BATCH" ]; then
    $UNSHAR -d $MAPDIR -n $NEWSGROUPS -s $NEWSSPOOL < $BATCH_TMP >> $ERROR_LOG
    STATUS=$?
else
    tee -a $BATCH_TMP | $UNSHAR -d $MAPDIR >> $ERROR_LOG
    STATUS=$?
fi

# note if we unpacked anything
if [ -s "$BATCH_TMP" ]; then
    touch $REBUILD
fi

# log the activity
#
cat $ERROR_LOG >> $LOG

# post processing - look for errors to report
#
if [ "$STATUS" -ne 0 ]; then
    # form the mail message header
    (if [ "$USERNAME" != "-" ]; then
	echo "To: $USERNAME"
	echo "Subject: getmap error"
	echo ""
	echo ""
     fi
     echo "getmap error log follows -----"
     cat $ERROR_LOG
     echo "end of getmap error log ====="
     echo ""
     echo ""
     if [ ! -z "$BATCH" ]; then
         echo "$BATCH_TMP work queue follows -----"
         cat $BATCH_TMP
         echo "end of $BATCH_TMP work queue ====="
     else
         echo "[stdin] work queue follows -----"
	 cat $BATCH_TMP
         echo "end of [stdin] work queue ====="
     fi) | eval "$REPORT"
else
    rm -f $ERROR_LOG	# no error, so remove the error log
fi
rm -f $BATCH_TMP

exit 0
