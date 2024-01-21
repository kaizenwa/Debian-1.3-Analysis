#! /bin/sh

# Build the config.h file, using the buildconfig program, first ensuring that
# it exists. 22-May-1996: remove the use of the "-a" flag for /bin/sh because
# it is not implemented in the FreeBSD shell. Sigh.

make buildconfig || exit 1

(sed -n '/\$/d;s/^\([A-Z][^:	 ]*\)[	 ]*=[	 ]*\([^  ]*\)[	 ]*$/\1=\2 export \1/p' \
  < makefile ; echo "./buildconfig") | /bin/sh || exit
echo ">>> config.h built"
echo " "

# End of Configure-config.h
