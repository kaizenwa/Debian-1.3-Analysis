#! /bin/sh
:
#ident	"@(#)smail/util:RELEASE-3_2:logsumm.sh,v 1.1 1996/03/09 22:30:00 woods Exp"
#
# 	logsumm - daily logfile summary for smail-3.2
#
# Attempts to summarize log files generated when SMAIL_LOG_STYLE=2

# common location
PATH="X_UTIL_PATH_X:X_SECURE_PATH_X"; export PATH

SMAIL_PROGRAM="X_SMAIL_NAME_X"
MAILLOG="`$SMAIL_PROGRAM -bP logfile`"

AWK=${AWK:-awk}

# this is "efficient" only for batch operations....
#
# Sort records so that "Completed" are first, "Delivered" are in the
# middle, and "Received" come last.
#
sort +2 -4 $MAILLOG | $AWK '

BEGIN {
	COMPLETE = 0;
	size = 0;
	from = ""
	tocnt = 0;
	tolist[0] = "";
}

$4 ~ /Completed/ {
	COMPLETE = 1;
	tocnt = 0;
	tolist[0] = "";
	next;
}

$4 ~ /Delivered/ {
	if (COMPLETE) {
		for (i = 5; i <= NF; i++) {
			if (substr($i, 1, 7) == "ORIG-TO") {
				tocnt += 1;
				tolist[tocnt] = substr($i, 9);
			}
		}
	} else {
		print;
	}
	next;
}

$4 ~ /Failed/ {
	if (COMPLETE) {
		tocnt += 1;
		tolist[tocnt] = "FAILED:" substr($5, 4);
	} else {
		print;
	}
	next;
}

$4 ~ /Deferred/ {
	if (COMPLETE) {
		tocnt += 1;
		tolist[tocnt] = "DEFERRED:" substr($5, 4);
	} else {
		print;
	}
	next;
}

$4 ~ /Returned/ && $5 ~ /error/ {
	if (COMPLETE) {
		tocnt += 1;
		tolist[tocnt] = "RETURNED-TO:" substr($7, 4);
	} else {
		print;
	}
	next;
}

$4 ~ /Received/ {
	if (COMPLETE) {
		from = substr($5, 6);
		for (i = 5; i <= NF; i++)	 {
			if (substr($i, 1, 4) == "SIZE") {
				size = substr($i, 6) + 0;
			}
		}
		printf("%s %s %07d %s => ", $1, $2, size, from);
		for (i = 1; i <= tocnt; i++) {
			printf("%s ", tolist[i]);
		}
		print "";
	} else {
		print;
	}
	next;
}

# un-important statistics....
#
$4 ~ /destination/ && $5 ~ /supports/ {
	next;
}

# spew out anything that is left over....
#
{
	print;
}

'

exit 0
