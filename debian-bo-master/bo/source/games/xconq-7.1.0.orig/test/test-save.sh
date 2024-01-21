#!/bin/csh
# Iterate through all library modules, comparing save and restore.

set srcdir = $1

set logname = savetest.log

/bin/rm -f $logname
echo Test started on `date` > $logname
foreach i ( $srcdir/../lib/*.g )
	echo $i
	echo "" >>$logname
	echo ">>> Running skelconq on: " $i "<<<" >>$logname
	echo "" >>$logname
	../kernel/skelconq -f $i -L $srcdir/../lib <$srcdir/savetst1.inp >>&$logname
	mv save.xconq save1.xconq
	echo ">>> Restoring skelconq; <<<" >>$logname
	../kernel/skelconq -f save1.xconq <$srcdir/savetst2.inp >>&$logname
	echo ">>> diff -c save1.xconq save.xconq <<<" >>$logname
	diff -c save1.xconq save.xconq >>$logname
	echo `diff save1.xconq save.xconq | wc -l` lines of difference
end
echo Test finished on `date` >> $logname
