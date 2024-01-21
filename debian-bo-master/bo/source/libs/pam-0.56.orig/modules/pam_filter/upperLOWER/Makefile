#
# $Id: Makefile,v 1.4 1997/01/04 20:25:04 morgan Exp $
#
# $Log: Makefile,v $
# Revision 1.4  1997/01/04 20:25:04  morgan
# removed need for make
#
# Revision 1.3  1996/11/10 20:13:08  morgan
# email address
#
# Revision 1.2  1996/11/10 20:12:24  morgan
# cross platform support
#
# Revision 1.1  1996/06/02 08:17:02  morgan
# Initial revision
#
#
# This directory contains a pam_filter filter executable
#
# Created by Andrew Morgan <morgan@parc.power.net> 1996/3/11
#

TITLE=upperLOWER

#

OBJS = $(TITLE).o

####################### don't edit below #######################

dummy:
	@echo "**** This is not a top-level Makefile "

all: $(TITLE)

$(TITLE): $(OBJS)
	$(CC) -o $(TITLE) $(OBJS)
	strip $(TITLE)

install:
	$(MKDIR) $(FILTERSDIR)
	$(INSTALL) -m 511 $(TITLE) $(FILTERSDIR)

remove:
	cd $(FILTERSDIR) && rm -f $(TITLE)

clean:
	rm -f $(TITLE) $(OBJS) core *~

extraclean: clean
	rm -f *.bak

.c.o:	
	$(CC) $(CFLAGS) -c $<

