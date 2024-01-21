
BINDIR		= $(DESTDIR)/usr/local/etc
MANDIR		= $(DESTDIR)/usr/local/man
MANEXT		= 8

INSTALL		= /usr/local/etc/install -c

PROG		= fixmount

FILES		= README Makefile $(PROG).8 \
		  $(SRCS) $(MOUNTX) $(MOUNTH) fixrmtab

MOUNTX		= mount.x
MOUNTH		= mount.h
SRCS		= $(PROG).c mount_xdr.c clnt_generic.c ualarm.c
OBJS		= $(PROG).o mount_xdr.o $(EXTRAS)

#ifdef ultrix
CFLAGS		= -O
LIBS		=
EXTRAS		= ualarm.o clnt_generic.o
#elifdef sgi
CFLAGS		= -O
# ifndef SVR4
LIBS		= -lsun -lc_s
# endif
EXTRAS		= ualarm.o
#elifdef SVR4
CFLAGS		= -O -DSVR4
#LIBS		= -lsocket -lnsl
# need in SunOS 5.3 for reserved port rpc to work
LIBS		= -L/usr/ucblib -lrpcsoc -R/usr/ucblib -lsocket -lnsl
EXTRAS		= ualarm.o
#else
CFLAGS		= -O # -g
LIBS		=	
EXTRAS		=
#endif

$(PROG): $(PROG).o $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS)

fixmount.o mount_xdr.o: mount.h

mount.h: $(MOUNTX)
	rpcgen -h $(MOUNTX) > $@

mount_xdr.c: $(MOUNTX)
	rpcgen -c $(MOUNTX) > $@

install: $(PROG)
	$(INSTALL) -s $(PROG) $(BINDIR)
	$(INSTALL) -m 444 $(PROG).8 $(MANDIR)/man$(MANEXT)/$(PROG).$(MANEXT)
	$(INSTALL) fixrmtab $(BINDIR)

shar:	$(FILES)
	shar $(FILES) > $(PROG).shar

clean:
	rm -f $(PROG) *.o core

realclean: clean
	rm -f mount.h mount_xdr.c

