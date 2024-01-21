#! /bin/sh
# first send telnet negotiation to set "mode char" i.e
# IAC WILL SUPPRESS-NO-GA IAC WILL ECHO IAC DONT ECHO
PATH=/local/bin/adm:/local/bin:/usr/bin:/bin
export PATH
awk 'END {printf "%c%c%c%c%c%c%c%c%c", 255, 251, 3, 255, 251, 1, 255, 254, 1}' /dev/null
echo `date`"	$$ connection opened from "`getpeername` >> /var/log/onpty
stelnet -f | onpty /bin/login | stelnet -t
echo `date`"	$$ connection closed." >> /var/log/onpty
