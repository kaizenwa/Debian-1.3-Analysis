#! /bin/sh
:
#ident	"@(#)smail/src:RELEASE-3_2:bump_cnt.sh,v 1.5 1996/06/21 19:19:48 woods Exp"

#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# incremement the compile counter and set the compilation date
if [ ! -f ldinfo.c ]; then
	compile_num=0
else
	compile_num=`sed -n 's/^int compile_num = \(.*\);/\1/p' < ldinfo.c`
	if [ ! "$compile_num" ]; then compile_num=0; fi
fi
compile_num=`expr $compile_num + 1`
compile_date=`date | awk '{ print $6 "-" $2 "-" $3 }'`
rm -f ldinfo.c
cat > ldinfo.c <<EOF
/*
 * This file defines the number of compiles since the first time this
 * file was created when compiling smail.  This information is used in
 * expand.c to define the values for \$compile_num and \$compile_date.
 * To reset the compilation count, simply remove ldinfo.c.
 */
int compile_num = $compile_num;
char *compile_date = "$compile_date";
EOF

echo "Compile #$compile_num on $compile_date"

exit 0
