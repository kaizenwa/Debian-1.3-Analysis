#!/bin/sh
#
# Sample hlfsd startup script, used at cs.columbia.edu.
#	Erez Zadok <ezk@cs.columbia.edu>
#
cd /
if [ -h /usr/spool/mail -a -x /usr/etc/hlfsd ]; then
	/usr/etc/hlfsd -a /var/spool/alt_mail -x all -l /var/log/hlfsd /mail/home .mailspool
	echo -n ' hlfsd'
fi
