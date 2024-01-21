#! /bin/sh

###
### This script would normally be integrated into Makefile.in, but
### unfortunately a stupid bug in the brain damaged ULTRIX make
### prevents me from doing it.  --tudor
###

diff prefix prefix.new > /dev/null

if test $? -ne 0; then
	echo "prefix changed from `cat prefix` to `cat prefix.new`"
	echo "affected files: $1"
	cp prefix.new prefix
	rm -f prefix.new
	rm -f "$1"
fi

exit 0
