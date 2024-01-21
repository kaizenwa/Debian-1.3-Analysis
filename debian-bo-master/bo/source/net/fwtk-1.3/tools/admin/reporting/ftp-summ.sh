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


TMPS="/tmp/.permnet /tmp/.permden /tmp/.permdtot /tmp/.permtot \
	/tmp/.permsrv /tmp/.bytesin /tmp/.bytesout /tmp/.totalin /tmp/.totalout"
rm -f $TMPS

grep 'ftp-gw.*:' | awk '

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
			printf("%d %s\n",permits[c],c) >> "/tmp/.permnet";
	}
	printf("%d\n",total_permitted) > "/tmp/.permtot";


	if(total_denied > 0) {
		for(c in denies)
			printf("%d %s\n",denies[c],c) >> "/tmp/.permden";
	}
	printf("%d\n",total_denied) > "/tmp/.permdtot";


	if(total_input > 0) {
		for(c in bytesin)
			if(bytesin[c] > 0)
			printf("%d %s\n",bytesin[c],c) >> "/tmp/.bytesin";
	}
	printf("%d\n",total_input) > "/tmp/.totalin";


	if(total_output > 0) {
		for(c in bytesout)
			if(bytesout[c] > 0)
			printf("%d %s\n",bytesout[c],c) >> "/tmp/.bytesout";
	}
	printf("%d\n",total_output) > "/tmp/.totalout";
}'


if [ -s /tmp/.permnet -a -s /tmp/.permtot ]; then
echo
echo "FTP service users (total: `cat /tmp/.permtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.permnet | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.permden -a -s /tmp/.permdtot ]; then
echo
echo "Denied FTP service users (total: `cat /tmp/.permdtot`)"
echo "Connects      Host/Address"
echo "--------      ------------"
sort -r -n /tmp/.permden | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.totalout -a -s /tmp/.bytesout ]; then
echo
echo "FTP service output thruput (total Kbytes: `cat /tmp/.totalout`)"
echo "KBytes        Host/Address"
echo "------        ------------"
sort -r -n /tmp/.bytesout | awk '{ printf("%-13d %s\n",$1,$2); }'
fi


if [ -s /tmp/.totalin -a -s /tmp/.bytesin ]; then
echo
echo "FTP service input thruput (total Kbytes: `cat /tmp/.totalin`)"
echo "KBytes        Host/Address"
echo "------        ------------"
sort -r -n /tmp/.bytesin | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

rm -f $TMPS
