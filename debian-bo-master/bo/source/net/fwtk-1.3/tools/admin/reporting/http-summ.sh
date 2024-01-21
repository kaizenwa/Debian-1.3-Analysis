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


TMPS="/tmp/.zpermnet /tmp/.zpermden /tmp/.zpermdtot /tmp/.zpermtot \
	/tmp/.zpermsrv /tmp/.zbytesin /tmp/.zbytesout /tmp/.ztotalin \
	/tmp/.ztotalout"
rm -f $TMPS

grep 'http-gw.*:' | awk '

BEGIN {
	total_permitted = 0;
	total_phosts = 0;
	total_denied = 0;
	total_dhosts = 0;
	total_input = 0;
	total_output = 0;
}

$6 == "deny" {
	total_denied++;
	x = substr($7,6);
	if(!denies[x])
		total_dhosts++;
	denies[x]++;
}

$6 == "exit" {
	x = substr($7,6);
	total_permitted++;
	if(!permits[x])
		total_phosts++;
	permits[x]++;
	bytesin[x] += substr($9,4) / 1024;
	total_input += substr($9,4) / 1024;
	bytesout[x] += substr($10,5) / 1024;
	total_output += substr($10,5) / 1024;
}

END {
	if(total_permitted > 0) {
		for(c in permits)
			printf("%d %s\n",permits[c],c) >> "/tmp/.zpermnet";
	}
	printf("%d\n",total_permitted) > "/tmp/.zpermtot";


	if(total_denied > 0) {
		for(c in denies)
			printf("%d %s\n",denies[c],c) >> "/tmp/.zpermden";
	}
	printf("%d\n",total_denied) > "/tmp/.zpermdtot";


	if(total_input > 0) {
		for(c in bytesin)
			if(bytesin[c] > 0)
			printf("%d %s\n",bytesin[c],c) >> "/tmp/.zbytesin";
	}
	printf("%d\n",total_input) > "/tmp/.ztotalin";


	if(total_output > 0) {
		for(c in bytesout)
			if(bytesout[c] > 0)
			printf("%d %s\n",bytesout[c],c) >> "/tmp/.zbytesout";
	}
	printf("%d\n",total_output) > "/tmp/.ztotalout";
}'


if [ -s /tmp/.zpermnet -a -s /tmp/.zpermtot ]; then
echo
echo "HTTP service users (total: `cat /tmp/.zpermtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.zpermnet | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.zpermden -a -s /tmp/.zpermdtot ]; then
echo
echo "Denied HTTP service users (total: `cat /tmp/.zpermdtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.zpermden | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.ztotalout -a -s /tmp/.zbytesout ]; then
echo
echo "HTTP service output thruput (total Kbytes: `cat /tmp/.ztotalout`)"
echo "KBytes        Host/Address"
echo "------        ------------"
sort -r -n /tmp/.zbytesout | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.ztotalin -a -s /tmp/.zbytesin ]; then
echo
echo "HTTP service input thruput (total Kbytes: `cat /tmp/.ztotalin`)"
echo "KBytes        Host/Address"
echo "------        ------------"
sort -r -n /tmp/.zbytesin | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

rm -f $TMPS
