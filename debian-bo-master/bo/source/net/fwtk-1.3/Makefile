#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.4 94/11/01 11:53:25 mjr rel "
#
# Modified 1995-12-12 Michael Shields <shields@crosslink.net>


# Master Makefile is included from toplevel directory
include	Makefile.config


# directories to build libraries (and maybe executables) in
# >>>these must be built before the others<<<
LIB=	lib auth


# directories to build executables in
DIRS=	smap smapd netacl plug-gw ftp-gw tn-gw rlogin-gw http-gw doc/man # x-gw


all:
	for a in $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

clean:
	for a in $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

install:
	if [ ! -d $(DEST) ]; then \
		mkdir $(DEST); \
	fi
	for a in config $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done
