#! /bin/sh
#
# NET-BASE	A collection of programs that form the base set of the
#		NET-2 Networking Distribution for the LINUX operating
#		system.
#
# Usage:	Install.sh [--devices] [--firsttime] [--nobackup]
#
# Version:	@(#)Install.sh  1.62	01/20/94
#
# Authors:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
#		Johannes Grosen, <grosen@argv.cs.ndsu.nodak.edu>
#		Copyright 1988-1993 MicroWalt Corporation
#
#		This program is free software; you can redistribute it
#		and/or  modify it under  the terms of  the GNU General
#		Public  License as  published  by  the  Free  Software
#		Foundation;  either  version 2 of the License, or  (at
#		your option) any later version.
#
PATH=/bin:/usr/bin:/sbin:/usr/sbin
export PATH
NROFF="groff -Tascii"	# how to format man pages for catman

BIN_PROGS="hostname netstat dnsdomainname"
SBIN_PROGS="arp ifconfig rarp route slattach ipfw"

  backup()
  {
	if [ "${nobackup}" = "NO" ]; then
		if [ -s $1 ]; then
			mv $1 $1.old
		fi
	fi
  }

  # Display a prompt, and then ask for Y or N.
  yesno() \
  {
	if [ $# = 0 ]; then
		prompt="Are you sure? (y/n): "
	  else
		prompt="$*: "
	fi
	while true
	do
		echo -n ${prompt}
		read answer
		if [ -n "${answer}" -a \
		     "${answer}" = "y" -o "${answer}" = "Y" -o \
		     "${answer}" = "n" -o "${answer}" = "N" ]; then
			break
		fi
		echo "***** WRONG ANSWER!"
	done
	if [ "${answer}" != "y" -a "${answer}" != "Y" ]; then
		return 1
	  else
		return 0
	fi
  }


  nobackup=NO
  while [ $# != 0 ]; do
	case $1 in
		--nobackup)
			nobackup="YES"
			shift
			;;

		-*)
			echo "Usage: Install.sh [--nobackup]" >&2
			exit 1
			;;

		*)
			shift
			;;
	esac
  done
  echo
  echo "This procedure will install the NET-3 Base Utilities for you."

  ETC="/etc"
  SBIN="/sbin"
  CONF="/etc"
  DOLINK=0
  DIRS="/dev /sbin /etc /bin /usr /usr/bin /usr/man"


  # Are we expected to install preformatted manual pages?
  CATMAN=0
  if [ -d /usr/man/cat1 ]; then
	echo
	if yesno "Do you want me to pre-format the manual pages? (y/n)"; then
		CATMAN=1
	fi
  fi

  # Install the primary user commands.
  echo ; echo "Installing USER commands:"
  for i in ${BIN_PROGS}
  do
	path=/bin/`basename $i`
	echo -n "${path} "
	backup ${path}
	cp $i ${path}
	chmod 511 ${path}
	chown bin.bin ${path}
  done
  (echo -n "/bin/dnsdomainname"; cd /bin; ln -f hostname dnsdomainname ; echo)

  # Install the primary system administrator commands.
  echo ; echo "Installing SYSTEM administrator commands:"
  for i in ${SBIN_PROGS}
  do
	path=${SBIN}/`basename $i`
	echo -n "${path} "
	backup ${path}
	cp $i ${path}
	chmod 500 ${path}
	chown bin.bin ${path}
  done
  echo

  ISNLS=0
  if [ -f ./config.h ]; then
    ISNLS=`cat ./config.h | grep "[define][ ]*[NLS]" | awk '{ print $3 }'`
  fi

  if [ ${ISNLS} -eq 1 ]; then
    echo "----------------------------------------------------------------------------"
    echo "Now, (try to) install the manual pages and catalog file according to your"
    echo "language, described through your LANG environment variable ..."
    echo "If this variable is not properly set, or your language is not yet supported,"
    echo "default language will be US."
    echo "----------------------------------------------------------------------------"
  else
    LANG=en_US.88591
  fi

  echo

  if [ ${ISNLS} -eq 1 ]; then
    if [ "${LANG}" = "" ]; then
	echo "*** LANG environment variable not set!"
	echo "*** Set default value to en_US.88591"
	LANG=en_US.88591

    elif [ ! -f nls/${LANG}/nettools.cat ]; then
	echo "*** Your language is currently not supported!"
	echo "*** Set default value to en_US.88591"
	LANG=en_US.88591
    fi
  fi

# Install the manual pages.
  echo ; echo -n "Installing manual pages..."
  for i in 1 5 8
  do
	# Install manual sources.
	chmod 644 man/${LANG}/*.$i
	chown bin.bin man/${LANG}/*.$i
	cp -af man/${LANG}/*.$i /usr/man/man$i >/dev/null

	# Do we need to pre-format them as well?
	if [ ${CATMAN} -eq 1 ]; then
		for manpage in man/${LANG}/*.$i
		do
			page=`basename ${manpage}`
			rm -f /usr/man/cat${i}/${page}
			${NROFF} -man $manpage > /usr/man/cat${i}/${page}
			chmod 644 /usr/man/cat${i}/${page}
			chown bin.bin /usr/man/cat${i}/${page}
		done
	fi
  done
  (cd /usr/man/man1; ln -sf hostname.1 domainname.1)
  echo

  # installation of NLS catalog files

  if [ ${ISNLS} -eq 1 ]; then
    echo; echo -n "Installing NLS catalog file..."

    cp nls/${LANG}/nettools.cat /usr/lib/locale/${LANG}/.
  fi

  echo ; echo "Installation completed."
  if [ "${nobackup}" = "NO" ]; then
	echo
	echo "Your old binaries (if they existed) were backed up to <name>.old."
	echo "You may safely delete them once you are satisfied that the new"
	echo "setup is working correctly."
  fi
  echo

  exit 0
