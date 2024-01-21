#! /bin/sh
#
# GNU version of BSD newvers.sh command
#
# Author: Jukka Virtanen <jtv@hut.fi>
# Date:   Sat Aug 12 00:07:09 1989
# Last modified: Thu Mar 23 22:18:13 1995
#
# Works in BSD & USG
# reads an optional config file with additional %TOKEN% definitions.
#
# The second optional parameter is the protofile to use instead of
# Version.c
#
# The optional configuration file name is given in the command line.
#
# The configuration file should contain a list of variable names in a
# shell variable VARLIST
#
# All the variables in this list separated by '%' signs, e.g.
#
# VARLIST="FOO FOO2"
# then all occurrences of %FOO% and %FOO2% are replaced by the values
# of FOO and FOO2 correspondingly.
#

if [ ! -r version ]; then
  echo 0 > version
fi

VERSIONPROTO=Version.c

VERSION=`cat version`

if [ -f /bin/hostname ]; then
  HOST=`/bin/hostname`
elif [ -f /bin/uname ]; then
  HOST=`/bin/uname -n`
else
  HOST="unknown.host.name"
  echo "$0: Unable to find out name of this host"
fi

if [ -z "$USER" ]; then
  USER=`whoami`
  if [ -z "$USER" ]; then
	  USER="anonymous"
	  echo "$0: Unable to find out user name"
  fi
fi

DIR=`pwd`
NOW=`date`

rm -f  version.c version.o

# Clear out the VARLIST variable
VARLIST=

# Read in the contents of the optional config file
if [ -f "$1" ]; then
  . ./$1
fi

# If the second argument exists, it is the protofile instead of ${VERSIONPROTO}
if [ -f "$2" ]; then
  VERSIONPROTO=$2
fi

#
# %VERSION%     includes DATE to be compatible with BSD newvers.sh
# %VERSIONONLY% can be used to include version without DATE
#
sed -e "s^%WHOANDWHERE%^${USER}@${HOST}:${DIR}^" \
    -e "s^%VERSION%^#${VERSION}: ${NOW}^"	 \
    -e "s^%WHO%^${USER}@${HOST}^"		 \
    -e "s^%WHERE%^${DIR}^"			 \
    -e "s^%DATE%^${NOW}^"			 \
    -e "s^%VARIABLE%^${VARIABLE}^"		 \
    -e "s^%VERSIONONLY%^#${VERSION}^"		 \
    ${VERSIONPROTO} > version.c

#
# According to VARLIST set in the config file, edit the version.c
#
if [ -n "$VARLIST" ]; then
 for var in $VARLIST; do
  eval val=\${${var}}
  sed -e "s^%$var%^$val^" version.c > /tmp/vers.$$
  mv /tmp/vers.$$ version.c
 done
fi

# Actually, this is funny, because file version always contains the
# next version, not the current one. This, too, is for compatibility.
echo `expr ${VERSION} + 1` > version

exit 0
