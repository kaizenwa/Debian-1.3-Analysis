#!/bin/sh
#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#

# THIS SCRIPT EXPECTS TO READ syslog() style log entries on its
# standard input

# how many do you want?
TOP=100

rm -f /tmp/.senders /tmp/.recip /tmp/.totmsg /tmp/.totkb

# cleanup and normalize
grep 'smap\[.*:' | tr 'A-Z' 'a-z' | sed -e 's/<//g' -e 's/>//g' | awk '

BEGIN {
	total_msgs = 0;
	total_kb = 0;
}


substr($6,0,5) == "host=" {
	size = substr($7,7);
	sender=substr($8,6);
	recip=substr($9,4);
	frombytes[sender] += size;
	tobytes[recip] += size;
	frommsg[sender]++;
	tomsg[recip]++;
	total_msgs++;
	total_kb += size / 1024;
}


END {
	if(total_msgs > 0) {
		for(c in frommsg)
			printf("%6d %8.1f   %-20s\n",frommsg[c],frombytes[c] / 1024,c) >> "/tmp/.senders";

		for(c in tomsg)
			printf("%6d %8.1f   %-20s\n",tomsg[c],tobytes[c] / 1024,c) >> "/tmp/.recip";

		printf("%d\n",total_msgs) > "/tmp/.totmsg";
		printf("%d\n",total_kb) > "/tmp/.totkb";
	}
}'

if [ -s /tmp/.totmsg ]; then
echo
echo "Total messages: `cat /tmp/.totmsg`  (`cat /tmp/.totkb` Kb)"
echo
echo "Top $TOP mail recipients (in messages)"
echo "Messages"
echo " Count      Kb    Address"
echo " -----      --    -------"
sort -r -n /tmp/.recip | head -$TOP

echo
echo "Top $TOP mail senders (in messages)"
echo "Messages"
echo " Count      Kb    Address"
echo " -----      --    -------"
sort -r -n /tmp/.senders | head -$TOP

echo
echo "Top $TOP mail recipients (in kilobytes)"
echo "Messages"
echo " Count      Kb    Address"
echo " -----      --    -------"
sort -r -n +1 /tmp/.recip | head -$TOP

echo
echo "Top $TOP mail senders (in kilobytes)"
echo "Messages"
echo " Count      Kb    Address"
echo " -----      --    -------"
sort -r -n +1 /tmp/.senders | head -$TOP
fi
rm -f /tmp/.senders /tmp/.recip /tmp/.totmsg /tmp/.totkb
