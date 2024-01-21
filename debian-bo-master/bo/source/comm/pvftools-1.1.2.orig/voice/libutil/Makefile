#
# Makefile
#
# This is the Makefile for libutil.
#
# $Id: Makefile,v 1.8 1997/01/22 07:35:38 marc Exp $
#

UTILOBJS=check_system.o config.o no_modem.o make_path.o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

all: libutil.a

config.o: config.c
	$(CC) $(CFLAGS) -DCONF_DIR=\"$(CONFDIR)\" -c $*.c

libutil.a: $(UTILOBJS)
	$(RM) -f libutil.a
	$(AR) $(ARFLAGS) libutil.a $(UTILOBJS)
	-$(RANLIB) libutil.a

install:

clean:
	@$(RM) -f libutil.a $(UTILOBJS)

fullclean: clean

include depend
