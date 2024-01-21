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

rm -f /tmp/.permtn /tmp/.desttn /tmp/.tnpermtot /tmp/.tnpermden /tmp/.tnpermdtot


egrep 'tn-gw.*:|rlogin-gw.*:' | awk '

BEGIN {
	total_permitted = 0;
	total_phosts = 0;
	total_denied = 0;
	total_dhosts = 0;
}


$6 == "exit" {
	total_permitted++;
	x = substr($7,6);
	if(!permits[x])
		total_phosts++;
	permits[x]++;
	dests[substr($8,6)]++;
	inbyt[x] += substr($9,4);
	outbyt[x] += substr($10,5);
}


$6 == "deny" {
	total_denied++;
	x = substr($7,6);
	if(!denies[x])
		total_dhosts++;
	denies[x]++;
}



END {
	if(total_permitted > 0) {
		for(c in permits)
			printf("%6d        %20.20s  %8d  %8d  %8d\n",permits[c],c,inbyt[c],outbyt[c],(inbyt[c] + outbyt[c])) >> "/tmp/.permtn";
		for(c in dests)
			printf("%6d        %-20.20s\n",dests[c],c) >> "/tmp/.desttn";
		printf("%d\n",total_permitted) > "/tmp/.tnpermtot";
	}
	if(total_denied > 0) {
		for(c in denies)
			printf("%6d        %-20.20s\n",denies[c],c) >> "/tmp/.tnpermden";
		printf("%d\n",total_denied) > "/tmp/.tnpermdtot";
	}
}'
if [ -s /tmp/.permtn ]; then
echo
echo "Top $TOP telnet gateway clients (total: `cat /tmp/.tnpermtot`)"
echo "Connects      Host/Address             Input    Output     Total"
echo "--------      ------------             -----    ------     -----"
sort -r -n +1 /tmp/.permtn | head -$TOP

echo
echo "Top $TOP telnet gateway clients in terms of traffic"
echo "Connects      Host/Address             Input    Output     Total"
echo "--------      ------------             -----    ------     -----"
sort -r -n +3 /tmp/.permtn | head -$TOP
fi


if [ -s /tmp/.tnpermden ]; then
echo
echo "Top $TOP Denied telnet gateway clients (total: `cat /tmp/.tnpermdtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.tnpermden | head -$TOP
fi
rm -f /tmp/.permtn /tmp/.desttn /tmp/.tnpermtot /tmp/.tnpermden /tmp/.tnpermdtot
