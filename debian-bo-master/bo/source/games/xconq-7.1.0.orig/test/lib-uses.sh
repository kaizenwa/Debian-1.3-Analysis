#!/bin/sh

# This script counts the references to GDL symbols in all Xconq library games.

cd $1/..

/bin/rm -rf /tmp/xc7

mkdir -p /tmp/xc7/lib
mkdir -p /tmp/xc7/test

for libfile in lib/*.g test/*.g ; do
	sed -e 's/"[^"]*"/ /g' $libfile | sed -e 's/;.*$//g' | tr '() ' '\012' >/tmp/xc7/$libfile
done

/bin/rm -f /tmp/raw-uses

# Count uses of keywords.

for file in keyword.def action.def history.def plan.def task.def goal.def ; do
	for sym in `grep DEF_ kernel/${file} | sed -e 's/^[^"]*"\([^, ]*\)".*$/\1/' | grep -v '^zz-'` ; do
		rslt=`fgrep -c -x ${sym} /tmp/xc7/*/*.g | grep -v ':0$' | sed -e 's/:1$//' | sed -e 's,/tmp/xc7/[a-z]*/,,' | tr '\012' ' '`
		echo ${sym} '	' ${rslt}
	done
done

for file in utype.def mtype.def ttype.def table.def gvar.def ; do
	for sym in `grep DEF_ kernel/${file} | sed -e 's/^[^"]*"\([^, ]*\)".*$/\1/' | grep -v '^zz-'` ; do
		rslt=`fgrep -c -x ${sym} /tmp/xc7/*/*.g | grep -v ':0$' | sed -e 's/:1$//' | sed -e 's,/tmp/xc7/[a-z]*/,,' | tr '\012' ' '`
		echo ${sym} '	' ${rslt}
	done
done




