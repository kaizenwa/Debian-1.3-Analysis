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

TMPS="/tmp/.permnet /tmp/.permden /tmp/.permdtot /tmp/.permtot /tmp/.permsrv"
rm -f $TMPS

grep 'netacl.*:' | awk '

BEGIN {
	total_permitted = 0;
	total_phosts = 0;
	total_denied = 0;
	total_dhosts = 0;
}


$6 == "permit" {
	total_permitted++;
	x = substr($7,6);
	if(!permits[x])
		total_phosts++;
	permits[x]++;
	srvic[substr($8,9)]++;
}


$6 == "deny" {
	total_denied++;
	x = substr($7,6);
	if(!denies[x])
		total_dhosts++;
	denies[x]++;
	srvic[substr($8,9)]++;
}



END {
	if(total_permitted > 0) {
		for(c in permits)
			printf("%d %s\n",permits[c],c) >> "/tmp/.permnet";
	}
	printf("%d\n",total_permitted) > "/tmp/.permtot";
	if(total_denied > 0) {
		for(c in denies)
			printf("%d %s\n",denies[c],c) >> "/tmp/.permden";
	}
	printf("%d\n",total_denied) > "/tmp/.permdtot";
	if(total_permitted > 0) {
		for(c in srvic)
			printf("%d %s\n",srvic[c],c) >> "/tmp/.permsrv";
	}
}'

if [ -s /tmp/.permnet -a -s /tmp/.permtot ]; then
echo
echo "Top $TOP network service users (total: `cat /tmp/.permtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.permnet | head -$TOP | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

if [ -s /tmp/.permden -a -s /tmp/.permdtot ]; then
echo
echo "Top $TOP Denied network service users (total: `cat /tmp/.permdtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.permden | head -$TOP | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

if [ -s /tmp/.permsrv ]; then
echo
echo "Service Requests"
echo "Requests      Service"
echo "--------      -------"
sort -r -n /tmp/.permsrv | awk '{ printf("%-13d %s\n",$1,$2); }'
fi
rm -f $TMPS
