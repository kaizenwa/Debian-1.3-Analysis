# Makefile for procmail rc libraries
#
#    Copyright (C) 1995  Alan K. Stebbens <aks@hub.ucsb.edu>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
# These are the defaults
ROOTDIR		=	$(NETROOT)
LIBDIR 		=	$(NETLIB)

# Define these as the root and library path for local filesystems
LOCALROOT 	=	/usr/local
LOCALLIB	=	${ROOTDIR}/lib/procmail

# Define these as the root and library path for the fileserver filesystems
NETROOT		=	/eci/procmail
NETLIB		=	${ROOTDIR}/lib

# Set these variables to enable "install-ftp"
FTPHOST		=	root@hub
FTPHOME		=	/usr/home/ftp
FTPDESTDIR	=	/pub/mail/

# Set SHARFILES to the files to be included in the shar archive
SHARFILES = README Makefile GNU-LICENSE procmailrc-example \
		*.rc *.help *.info proclint Copyright

# Set to the default install behaviour: "install-net" or "install-local"
DEFAULT		= install-net install-home install-local install-ftp

SHARNAME	= procmail-lib


#
SHELL		= /bin/sh

default:
	@echo 'make install-home     Installs proclint to ~/bin'
	@echo 'make install-mail     Installs library recipes to ~/Mail'
	@echo 'make install-local    Installs library recipes to /usr/local'
	@echo 'make install-net      Installs library recipes to fileserver'
	@echo 'make install-ftp      Installs library recipes to ftp server'
	@echo 'make check            Check for discrepancies'
	@echo 'make check-home       Check against ~/Mail'
	@echo 'make install          Install the default: $(DEFAULT)'

install: $(DEFAULT)

install-home:	$(HOME)/bin/proclint install-mail

$(HOME)/bin/proclint:	proclint
	@(set -x ; cp $? $@ ; chmod 755 $@ )

install-mail:
	${MAKE} LIBDIR=$(HOME)/Mail install-lib

check-home check-mail:
	${MAKE} LIBDIR=$(HOME)/Mail check

install-local:
	${MAKE} ROOTDIR=$(LOCALROOT) install-lib

install-net:
	${MAKE} ROOTDIR=$(NETROOT) install-lib

install-lib:
	@for lib in *.rc *.help *.info ; do \
	    ${MAKE} SRC=$$lib DST=$(LIBDIR)/$$lib install-file ; \
	done

DST	=	.phony-dst
SRC	=	.phony-src

install-file:	$(DST)
$(DST):		$(SRC)
	@if [ -f $@ ]; then ( set -x ; mv $@ $@.old ) ; fi
	@(set -x ; cp $(SRC) $@ ; chmod 444 $@ )

check::
	@-for lib in *.rc *.help *.info ; do \
	    inslib=$(LIBDIR)/$$lib ; \
	    if [ -f $$inslib ]; then \
		(set -x ; diff -c $$inslib $$lib ) ; \
	    else \
		echo $$lib not installed. ; \
	    fi ; \
	done
	@-for inslib in $(LIBDIR)/*.rc ; do \
	    lib=`basename $$inslib` ; \
	    if [ ! -f $$lib ]; then \
		echo $$inslib has no source. ; \
	    fi ; \
	done

uu::	$(SHARNAME).shar.gz.uu
$(SHARNAME).shar.gz.uu:	$(SHARNAME).shar.gz
	uuencode $? < $? > $@

gzip::	$(SHARNAME).shar.gz
$(SHARNAME).shar.gz:	$(SHARNAME).shar
	gzip -f $?

shar::  $(SHARNAME).shar
$(SHARNAME).shar:	$(SHARFILES)
	cwd=`pwd` ; \
	cd ..; \
	dir=`basename $$cwd` ; \
	files=`echo "$(SHARFILES)" | \
	       tr -s ' \10' '\12\12' | \
	       sed -e "s=^=$$dir/=" | \
	       tr '\12' ' ' ` ; \
	shar $$files > $$dir/$@

install-ftp: shar
	rcp $(SHARNAME).shar $(FTPHOST):$(FTPHOME)$(FTPDESTDIR)
