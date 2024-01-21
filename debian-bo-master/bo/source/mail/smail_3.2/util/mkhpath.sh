#!/bin/sh
# @(#) mkhpath.sh,v 1.5 1992/07/11 11:40:06 tron Exp

#
#    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992 Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# mkhostpath - build a pathalias database from a 'hosts' table
#
# Usage: mkhostpath [-n netname] [-c cost] [-g gateway] [-d] [ - | filename ]
#
#	-n netname	- form a map (not pathalais output) name the net netname
#	-c cost		- set cost to gateway, or network is no gateway
#	-g gateway	- set the gateway for hosts to be gateway
#	-d		- only print lines that has domain hostnames
#	-		- read hosts table from stdin
#	filename	- read hosts table from filename
#	<no arg>	- read hosts from /etc/hosts
#
# The mkhostpath(8) command reads a hosts table from 'filename' or
# standard input if '-' is used, and constructs routes to that network.
# The hosts table is assumed to be in the format of the /etc/hosts
# database file.  That is, the first field (internet address) and any,
# domain based name (field containing a '.') are ignored.  The hosts
# 'localhost' and 'loghost' are also ignored.  Comment lines, beginning
# with a '#', and blank lines are also ignored.  If '-d' is specified
# and address does NOT contain an alias that is a domain based name,
# then that line is ignored.  (useful to ignore test lines)
#
# By default, mkhostpath(8) builds route table to the network in the format
# of the output of pathalias(8) with the '-i' option.  The '-n' network flag
# overrides this default.
#
# If a gateway is specified with the '-g' option, but the '-n' network
# option is not used, then all routes to the network (except for the route
# to the gateway) are prepended with the route "gateway!".  A route to from
# the localhost to the gateway is explicitly formed.
#
# If neither the '-g' gateway nor the '-n' network options are given,
# then direct routes from the localhost to each host are constructed.
#
# The mkhostpath command will build a pathalias map to the network if the
# name of the network is given via the '-n' option.  The construction of
# the map depends on if the '-g' gateway option is given.
#
# When the '-g' gateway and the '-n' network options are both used, then a
# route from the localhost to the gateway is established, and the gateway is
# inserted into the network list.  The localhost is not added to
# the network list, even if it appears as a sitename in the hosts table.
# The cost of the link between the localhost and the gateway is 'LOCAL',
# unless overridden by the '-c' cost option.  The cost of routes inside
# the network is fixed at 'LOCAL' and is not changed by the '-c' cost option.
#
# If no gateway is specified with the '-n' network option, then it is
# assumed that the localhost is in the network and thus the localhost
# is inserted into the network list.  The cost of routes inside the network
# is assumed to be 'LOCAL', unless changed by the '-c' cost option.
#
# The value of the '-c' option may be any valid pathalias(8) cost expression.
# The '-c' flag is ignored if the '-n' flag is not given.


# base directory for smail library/utility
#
# These directories should be owned by root, and only writeable by root
#
PATH="X_UTIL_PATH_X:X_SECURE_PATH_X"; export PATH
UTIL_BIN_DIR=X_UTIL_BIN_DIR_X
TMPDIR=X_TMP_DIR_X

# standard locations
#
DCASEHOST=dcasehost
SORT=sort
HOSTS=/etc/hosts
SMAIL=X_SMAIL_NAME_X
GETOPT=$UTIL_BIN_DIR/getopt

# find out who we are
hostname="`$SMAIL -bP uucp_name`"

# parse - need public domain getopt - XXX
#
gateway=
gate=
GATE=0
netname=
cost=LOCAL
dotline=0
PROG=$0
set -- `$GETOPT -n $PROG -q n:c:g:d $*`
if [ "$?" -ne 0 ]; then
	echo "usage: $PROG [-n netname] [-c cost] [-g gateway] [-d] [- | filename]" 2>&1
	exit 1
fi
for i in $*; do
	case "$i" in
	-g) gate="$2"; gateway="$gate!"; GATE=1; shift 2;;
	-n) netname="$2"; shift 2;;
	-c) cost="$2"; shift 2;;
	-d) dotline=1; shift;;
	--) shift; break;;
	esac
done

# select where input is to come from
#
if [ "$#" -eq 1 ]; then
	case "$1" in
	-)	cat ;;			# `-' then stdin
	*)	cat "$1" ;;		# filename then that file
	esac
else 
	cat $HOSTS		# no argument then /etc/hosts
fi |
sed 's/[	 ]*#.*$//' |	# strip #-style comments
if [ -z "$netname" ]; then

	# pathalias style output

	# Make entries for lines which contain more than just an
	# initial inet address, localhost or loghost 
	awk 'NF > 1 {
	    if (DOTLINE == 1) {
		found = 0;
		for (i = 2; i <= NF && found == 0; ++i) {
		    if ($i ~ /\./) {
			found = 1;
		    }
		}
		if (found == 0) {
		    continue;
		}
	    }
	    for (i = 2; i <= NF; i++) {
		if ($i !~ /\./ && $i != "'$hostname'" && $i != "localhost" && $i != "loghost" && $i != "'$gate'") {
		    if (length($i) < 8) {
			tab = "\t\t";
		    } else {
			tab = "\t";
		    }
		    if (GATE > 0) {
			print $i tab "'$gate'!"$i"!%s";
		    } else {
			print $i tab $i"!%s";
		    }
		}
		if ($i == "'$hostname'") {
		    if (length($i) < 8) {
			tab = "\t\t";
		    } else {
			tab = "\t";
		    }
		    print $i tab "!%s	0";
		}
	    }
	}
	END {
	    if (GATE > 0) {
		if (length("'$gate'") < 8) {
		    tab = "\t\t";
		} else {
		    tab = "\t";
		}
		print "'$gate'" tab "'$gate'!%s";
	    }
	}' GATE="$GATE" DOTLINE=$dotline - | $DCASEHOST | $SORT -u -T $TMPDIR

else

	# output in the format of pathalias map input 

	# output the initial map data
	echo "file { [mkhostpath] }"
	if [ "$GATE" -gt 0 ]; then
		echo "$hostname	$gate($cost)"
		# the gateway will be the first listed site
		first_site="$gate";
	else
		first_site="$hostname";
	fi

	# Make entries for lines which contain more than just an
	# initial inet address, localhost or loghost.  Force the
	# first_site to be first in the list.

	awk 'BEGIN {
	    line = "'$netname' = { '$first_site'";
	    line_len = length(line);
	}
	NF > 1 {
	    if (DOTLINE == 1) {
		found = 0;
		for (i = 2; i <= NF && found == 0; ++i) {
		    if ($i ~ /\./) {
			found = 1;
		    }
		}
		if (found == 0) {
		    continue;
		}
	    }
	    for (i = 2; i <= NF; i++) {
		if ($i !~ /\./ && $i != "localhost" && $i != "loghost" && $i != "'$first_site'" && $i != "'$hostname'") {
		    field_len = length($i);
		    if (field_len+line_len > 76) {
			print line",";
			line = "    "$i;
			line_len = 4 + field_len;
		    } else {
		        line = line", "$i;
			line_len += field_len + 2;
		    }
		}
	    }
	}
	END {
	    line2 = " }(LOCAL)";
	    field_len = length(line2);
	    if (field_len+line_len > 76) {
		print line;
		print "    " line2;
	    } else {
		print line line2;
	    }
	}' GATE="$GATE" DOTLINE=$dotline -

fi
exit 0
