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

TMPS="/tmp/.perauth /tmp/.denauth /tmp/.permdtot /tmp/.denatot /tmp/.permatot /tmp/.authop"
rm -f $TMPS

grep 'authsrv.*:' | awk '

BEGIN {
	total_permitted = 0;
	total_denied = 0;
}


$6 == "AUTHENTICATE" {
	total_permitted++;
	permits[$7]++;
}


$6 == "BADAUTH" {
	total_denied++;
	denies[$7]++;
}


$7 == "ADDED" || $7 == "RENAMED" || $7 == "DELETED" || $7 == "ENABLED" {
	printf("%s %s %s\n",$6,$7,$9) >> "/tmp/.authop";
}

$7 == "DISABLED" || $7 == "UN-GWIZ" || $7 == "GWIZ" || $7 == "WIZ" {
	printf("%s %s %s\n",$6,$7,$9) >> "/tmp/.authop";
}

$7 == "GROUP" {
	printf("%s %s %s %s\n",$6,$7,$9,$11) >> "/tmp/.authop";
}

$7 == "CHANGE" {
	printf("%s %s %s\n",$6,$8,$9) >> "/tmp/.authop";
}




END {
	if(total_permitted > 0) {
		for(c in permits)
			printf("%d %s\n",permits[c],c) >> "/tmp/.perauth";
		printf("%d\n",total_permitted) > "/tmp/.permatot";
	}
	if(total_denied > 0) {
		for(c in denies)
			printf("%d %s\n",denies[c],c) >> "/tmp/.denauth";
		printf("%d\n",total_denied) > "/tmp/.denatot";
	}
}'

if [ -s /tmp/.perauth -a -s /tmp/.permatot ]; then
echo
echo "Top $TOP permitted user authentications (total: `cat /tmp/.permatot`)"
echo "Logins        User ID"
echo "------        -------"
sort -r -n /tmp/.perauth | head -$TOP | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

if [ -s /tmp/.denauth -a -s /tmp/.denatot ]; then
echo
echo "Top $TOP failed user authentications (total: `cat /tmp/.denatot`)"
echo "Attempts      Username"
echo "--------      --------"
sort -r -n /tmp/.denauth | head -$TOP | awk '{ printf("%-13d %s\n",$1,$2); }'
fi

if [ -s /tmp/.authop ]; then
echo
echo "Authentication Managment Operations"
echo "-----------------------------------"
sort /tmp/.authop
fi

rm -f $TMPS
