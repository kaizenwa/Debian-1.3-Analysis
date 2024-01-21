#!/bin/sh
#
# weekly firewall activity report
# this is not designed to be efficient, and will make many passes
# through your log files.
#
# you may need to tailor this
LOGS="/var/log/messages"

echo; echo
echo 'Electronic Mail Usage'
echo '----------------------------------------------------------------'
cat $LOGS | /usr/local/etc/smap-summ.sh

echo; echo
echo 'User Logins'
echo '----------------------------------------------------------------'
cat $LOGS | /usr/local/etc/authsrv-summ.sh

echo; echo
echo 'FTP Proxy usage'
echo '----------------------------------------------------------------'
cat $LOGS | /usr/local/etc/ftp-summ.sh

echo; echo
echo 'Telnet/Rlogin Proxy Usage'
echo '----------------------------------------------------------------'
cat $LOGS | /usr/local/etc/tn-gw-summ.sh

echo; echo
echo 'Network Service Connections'
echo '----------------------------------------------------------------'
cat $LOGS | /usr/local/etc/netacl-summ.sh

