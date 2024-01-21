#!/bin/sh
#
#  Copyright (c) 1994 by 3M Corporation 
#  All rights reserved.
# 
# 
#	Author: Justus J. Addiss, 3M Health Information Systems
#

# THIS SCRIPT EXPECTS TO READ syslog() style log entries on its
# standard input

# how many do you want?
TOP=100

TMPS="/tmp/.permnet /tmp/.permden /tmp/.permdtot /tmp/.permtot
/tmp/.permsrv /tmp/.permalrt /tmp/.permauth"

rm -f $TMPS

awk '

BEGIN {
	total_permitted = 0;
	total_phosts = 0;
	total_denied = 0;
	total_dhosts = 0;
	total_alerts = 0;
	total_auth   = 0;

	# Read the /etc/services file to get names for port numbers
	while (getline < "/etc/services" > 0) {
	    if (NF >= 2 && match($0, "^[ 	]*#") == 0) {
		services[$2] = $1
	    }
	}
}


$6 == "permit" {
	total_permitted++;
	host = substr($7,6);		# remove the 'host='
	if (index($5, "tn-gw") == 1)
	    svc = "telnet"
	else if (index($5, "x-gw") == 1) {
	    svc = "X"
	    host = $9
	}
	else if (index($5, "ftp-gw") == 1)
	    svc = "ftp"
	else if (index($5, "rlogin-gw") == 1)
	    svc = "rlogin"
	else
	    svc = substr($8,9);		# remove the 'service='
	key = host ":" svc		# create the array key
	if (!permits[key])		# do we know about this host?
		total_phosts++;		# if not then update total hosts
	permits[key]++;			# update this hosts count
	srvic[svc]++;			# and update the service count
}


$6 == "deny" {
	total_denied++;
	host = substr($7,6);		# remove the 'host='
	if (index($5, "tn-gw") == 1)
	    svc = "telnet"
	else if (index($5, "x-gw") == 1) {
	    svc = "X"
	    host = $9
	}
	else if (index($5, "ftp-gw") == 1)
	    svc = "ftp"
	else if (index($5, "rlogin-gw") == 1)
	    svc = "rlogin"
	else
	    svc = substr($8,9);		# remove the 'service='
	key = host ":" svc		# create the array key
	if (!denies[key])		# seen this host before?
		total_dhosts++;		# if not then update the count
	denies[key]++;			# update this hosts count
	srvic[svc]++;			# update the services seen
}

$6 == "securityalert:" {
	ip = host = $9
	type = $7
	port = $13
	
	# Try to get a name for the IP address
	if (hosts[host] != "") {	# If we know the name already, use it
	    host = hosts[host]
	}
	else {
	    # Use 'nslookup' to get the name from the IP address
	    while ("nslookup 2>/dev/null " host | getline > 0) {
		if (match($1, "^Name:") != 0) {
		    name = $2
		    break
		}
	    }
	    # If we failed then the name is "unknown"
	    if (name == "") {
		name = "unknown"
	    }
	    hosts[host] = name
	    host = name
	}
	if (host != "mercury.hsi.com") {
		total_security += 1;
		svc = port "/" type
		if (services[svc] != "")
	    	    svc = services[svc] "/" type
		key = host "/" ip ": " svc
		if (!alerts[key])
			total_alerts += 1;	
		alerts[key] += 1;
	}
}

$6 == "BADAUTH" {
	total_auth += 1;
	name = $7
	proxy = substr($8, 2)
	if (index(proxy, ")"))
		proxy = substr(proxy, 1, length(proxy) - 1)
	if (length($9) > 0)
		host = substr($9, 1, length($9) - 1)
	else
		host = "none"
	key = proxy ": " host " - " name
	if (!badauth[key])
		total_badauth += 1
	badauth[key] += 1
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

	if (total_security > 0) {
	    for (c in alerts)
		printf("%d %s\n",alerts[c],c) >> "/tmp/.permalrt";
	}

	if (total_auth > 0) {
	    for (c in badauth)
		printf("%d %s\n",badauth[c],c) >> "/tmp/.permauth";
	}
}'

if [ -s /tmp/.permauth ]; then
echo
echo "Authentication Failures"
echo "Failures      Proxy: Host - ID"
echo "--------      ---------------"
sort -r -n /tmp/.permauth | awk '{ printf("%-13d %s %s - %s\n", $1, $2, $3, $5);}'
fi

if [ -s /tmp/.permalrt ]; then
echo
echo "Security alerts"
echo "Failures      Host: port/type"
echo "--------      ---------------"
sort +1 -2 +0nr -1 +3n -4 +3  /tmp/.permalrt | awk '{ printf("%-13d %s
%s\n", $1, $2, $3);}'
fi

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

