: /bin/sh
# @(#) recvd.sh,v 1.2 1990/10/24 05:19:45 tron Exp
#
# recvd - Deliver mail which has been received on this node over arcnet
#
# SYNOPSIS
#	recvd [ sleep-interval ]
#
# DESCRIPTION
#	The recvd shell script should be started from the /etc/rc
#	file for all machines on an arcnet that wish to receive
#	mail.  These machines must have secure networking disabled
#	as daemons cannot interactively enter passwords.
#
#	The shell script wakes up at intervals and looks for files
#	under the directory /usr/spool/smail/forpro of the form
#	hostname/done/msgid, where hostname is the name of a host
#	that sent a message, and msgid is a 14 character message
#	identifier which begins with a digit.  Each such file
#	should have a corresponding message file in hostname/msgid,
#	which contains SMTP commands to use for delivery.  Each
#	message file is delivered by calling smail, and then the
#	message file and the "done" file are removed.
#
#	The default interval between times that the script checks
#	for new messages is 5 minutes.  This can be changed by giving
#	a time in seconds as the first argument for the command.

FORPRO_SPOOL_DIR=/usr/spool/smail/forpro
SLEEP_TIME="${1-300}"
SMAIL=/usr/lib/sendmail

# change to the directory containing new mail
cd $FORPRO_SPOOL_DIR

while :; do
    find */done -type f -name '[0-9]*' -print | while read done_file; do
	message_file="`echo $done_file | sed 's,/done/,/,'`"
	if [ -f "$message_file" ]; then
	    cat "$message_file" | $SMAIL -bS
	    rm -f "$message_file"
	fi
	rm -f "$done_file"
    done
    sleep "$SLEEP_TIME"
done

exit 0
