#! /bin/sh
:
#ident	"@(#)smail/conf/lib:RELEASE-3_2:xexec.sh,v 1.5 1996/02/16 19:01:56 woods Exp"

# this silly thing makes it *look* like make is really running the command....

echo "	"${1+"$@"}
eval ${1+"$@"}
