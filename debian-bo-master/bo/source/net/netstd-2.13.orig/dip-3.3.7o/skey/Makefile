# Makefile - S/KEY v1.1b
#
# Neil M. Haller <nmh@thumper.bellcore.com>
# Philip R. Karn <karn@chicago.qualcomm.com>
# John S. Walden <jsw@thumper.bellcore.com>
# Scott Chasin <chasin@crimelab.com>

MANDIR  =  /usr/local/man/man8
DESTDIR =  /usr/local/bin
LIBDIR  = /usr/local/lib

CC      = gcc
CFLAGS	= -pipe -O2 #-DUSE_ECHO
#SYSLIBS = -lshadow -ldbm
SYSLIBS = -ldbm # For ELF - no shadow needed (included in libc).

LIB     =  libskey.a
PROGS   =  key keyinit keysh
LIBOBJS =  skeylogin.o skeysubr.o md4.o put.o
LIBSRCS =  skeylogin.c skeysubr.c md4.c put.c

what:
	@echo ""
	@echo "Please specify the type of system you are compiling on:"
	@echo ""
	@echo " bsd             Berkeley 4.x, 386BSD, Aviion DG/UX"
	@echo " linux		Linux"
	@echo " sunos4          Sun SunOS 4.x"
	@echo " sunos5          Sun SunOS 5.x (Solaris 2.x)"
	@echo " ultrix          Dec Ultrix 4.1.x"
	@echo " sysv            AT&T System V and clones"
	@echo " irix            Silicon Graphic's IRIX"
	@echo " next            NeXTStep 2.0"
	@echo " hpux            HP/UX Hewlett-Packard Unix v9.x"
	@echo ""
	@echo "Example: make sunos4"
	@echo ""

default:
	@make linux

all:	$(LIB) $(PROGS) done

setuid:
	chmod u+s keyinit keysh

install:
	chmod u+s keyinit keysh
	mv $(PROGS) $(DESTDIR)
	mv $(LIB) $(LIBDIR)

linux:
	make all SYS=sysv RANLIB="ranlib" CFLAGS="$(CFLAGS) -DSHADOW -DSYSV -DPOSIX"

sunos5:
	make all SYS=$@ RANLIB="echo" CFLAGS="$(CFLAGS) -DSYSV -DSOLARIS \
        -DSIGVOID"

sunos4:
	make all SYS=$@ RANLIB="/bin/ranlib" CFLAGS="$(CFLAGS) -DSUN"

ultrix:
	make all SYS=$@ RANLIB="/usr/bin/ranlib" CFLAGS="$(CFLAGS) -DULTRIX"

sysv:
	make all SYS=$@ RANLIB="echo" CFLAGS="$(CFLAGS) -DSYSV"

key:    skey.c $(LIB)
	$(CC) -o $@ $(CFLAGS) skey.c $(LIB) $(SYSLIBS)

keyinit: skeyinit.c $(LIB)
	$(CC) -o $@ $(CFLAGS) skeyinit.c $(LIB) $(SYSLIBS)

keysh:  skeysh.c $(LIB)
	$(CC) -o $@ $(CFLAGS) skeysh.c $(LIB) $(SYSLIBS)

$(LIB): $(LIBOBJS)
	ar rv $(LIB) $(LIBOBJS)
	$(RANLIB) $(LIB)

done:
	@echo "Make completed."

clean:
	rm -f key keyinit keysh *.o libskey.a *.bak *.BAK *.s tags
