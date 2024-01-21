#!/bin/sh
#ident	"@(#)hlfsd	1.0	93/09/12 EZK"

if [ ! -d /usr/bin ]
then			# /usr not mounted
	exit
fi

killproc() {		# kill the named process(es)
	pid=`/usr/bin/ps -e |
	     /usr/bin/grep $1 |
	     /usr/bin/sed -e 's/^  *//' -e 's/ .*//'`
	[ "$pid" != "" ] && kill $pid
}

PATH=/usr/bin

#
# Start/stop processes required for client NFS
#

case "$1" in
'start')
	#
	# Start the hlfsd mail redirector service
	#
	if [ -x /usr/lib/opt/hlfsd -a -h /var/mail ]
	then
		/usr/lib/opt/hlfsd -a /var/alt_mail -x all -l /var/tmp/hlfsd /mail/home .mailspool
	fi
	;;

'stop')
	killproc hlfsd
	;;
*)
	echo "Usage: /etc/init.d/hlfsd { start | stop }"
	;;
esac
