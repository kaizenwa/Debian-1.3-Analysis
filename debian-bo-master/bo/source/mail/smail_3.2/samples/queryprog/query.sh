#!/bin/sh
# @(#) query.sh,v 1.2 1990/10/24 05:21:26 tron Exp
#
# match hosts in a shell case statement.  It is generally more
# efficient to use a paths file and a method file to perform this kind
# of routing.
#
# See the source file samples/queryprog/routers for more information
# on how this shell script is to be used as part of a complete router
# file entry.

# The hostname is passed as the first argument, write a path and
# transport for each host that we match.  Alternately, no transport is
# output if the default is sufficient.
case "$1" in

\[*)	# look for internet addresses in square brackets
	inet=`echo "$1" | sed -n 's/^\[\([0-9.]*\)\]$/[\1]/p'`
	if [ "$inet" ]; then
		echo $inet smtp
	else
		exit 1
	fi;;
foo)	echo foo uusmtp;;
bar)	echo foo!bar uusmtp;;
curds)	echo curds;;
whey)	echo curds!whey;;
*' '*|*'	'*) exit 1;;	# watch out for hostnames with whitespace
*)	echo foo!$1 uusmtp;;	# forward mail for unknown hosts to foo

esac

exit 0
