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
# RcsId: "Header: Makefile,v 1.2 94/05/27 14:10:39 mjr Exp "

include	../../../Makefile.config

FILES=	authsrv-summ.sh ftp-summ.sh netacl-summ.sh smap-summ.sh \
	tn-gw-summ.sh daily-report.sh weekly-report.sh today-report.sh \
	login-summ.sh

# these are no-ops
all:
clean:

install:
	for a in $(FILES); do \
		$(CP) $$a $(DEST); \
		chmod 755 $(DEST)/$$a; \
	done
