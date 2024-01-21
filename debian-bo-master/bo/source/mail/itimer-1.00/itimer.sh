#!/bin/sh

# Alternative to the timer.c program.  This script does not keep
# track of elapsed time as well as the C program, e.g. the time
# between sleeps is ignored.  If all the timers have quick
# executing functions asociated with them, then this shouldn't be
# much of a problem.  But a long running timer function can push
# make other timers timeout much later than they should.

set `echo 0 0 0 0`

trap 'shift 3' 2

while :
do
	read time
	set `/bin/time sleep ${time:-5} 2>&1`
	echo $1
done
