#!/bin/sh

# This script counts the references to GDL symbols in all Xconq sources.

cd $1/..

# Count uses of keywords.

for file in keyword.def action.def history.def plan.def task.def goal.def ; do
	for sym in `grep '^DEF_' kernel/$file | sed -e 's/^[^,]*, \([a-zA-Z0-9_]*\)[,)].*$/\1/' | grep -v '^zz-'` ; do
		rslt=`grep -c "${sym}[^A-Za-z_]" */*.[hc] | grep -v ':0$' | sed -e 's/:1$//' | tr '\012' ' '`
		echo ${sym} '	' ${rslt}
	done
done

# Count uses of function-based symbols.

for file in utype.def mtype.def ttype.def table.def gvar.def ; do
	for fun in `grep '^DEF_' kernel/${file} | sed -e 's/^[^,]*, \([a-zA-Z0-9_]*\),.*$/\1/' | grep -v '^zz-'` ; do
		rslt=`grep -c $fun */*.[hc] | grep -v ':0$' | sed -e 's/:1$//' | tr '\012' ' '`
		echo ${fun} '	' ${rslt}
	done
done
