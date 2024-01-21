#!/bin/sh
# @(#) rcsmtp.sh,v 1.4 1992/09/06 04:16:11 tron Exp

# Receive compressed batches of SMTP commands and send them
# to smail.

# the following line should be changed to reflect the
# organization of your system.
/usr/local/bin/compress -d | /bin/rsmtp -oMr cbsmtp
exit 0
