SHELL = /bin/sh

#
# Following configurations were tested at least to compile
#

# GCC: SunOS 4.1.3/4.1.4 or Linux 1.2.13 (NetBSD x.xx?)
CC	=	gcc
CFLAGS	=	-I../include -O2 -Wall

# Solaris 2.5: SUNWspro C or GCC
#CC	=	cc
#CFLAGS	=	-I../include -xO2 -v
#CC	=	gcc
#CFLAGS	=	-I../include -O2 -Wall
#LDFLAGS =	-L/usr/ucblib -R/usr/ucblib -lrpcsoc -lnsl -lsocket

# Native C compiler on IRIX 5.3, HP-UX 9.01, HP-UX 10.10 or SunOS 4.1.3
#CC	=	cc
#CFLAGS	=	-I../include -O

# HPUX 10.20 Compiler
#CC=c89 -D_HPUX_SOURCE -Dhpux -DHPUX10

# For AIX3.2.5 (Not tested with p3nfsd 2.0 or newer)
#AIXLIBS  =	-lbsd
#AIX_OBJS =	mount_aix.o


# Default installdirectory applies to local installation at FAU
BINDIR	=	/proj/psion/bin/sun5


### no need to change anything below this line ###

MOBJS	= 	mp_main.o mp_mount.o nfs_prot_svc.o nfs_prot_xdr.o \
		mp_pfs_ops.o mp_serial.o mp_inode.o mp_xmit.o crc.o pty.o
PROGRAM	=	p3nfsd

OBJS	=	$(MOBJS) $(AIX_OBJS)

$(PROGRAM):$(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(AIXLIBS)

nfsc_S3.opl: nfsc.opl
	@rm -f $@
	echo >  $@ 'REM nfsc_S3 -- automatically created from nfsc -- do not edit' 
	sed >> $@ < $? -e 's/\(rsset:[        ]*([    ]*\)16,/\115,/' \
	    -e 's/uadd[     ]*(/myuadd%:(/g' 
	@chmod a-w $@
	@touch .$?

.nfsc.opl: nfsc_S3.opl
	@echo ERROR: $? has been modified. Please transfer 
	@echo ERROR: your changes back to `echo $@ | sed -e 's/^\.//'`
	@exit 1

opl: nfsc_S3.opl .nfsc.opl

install:$(PROGRAM)
	rm $(BINDIR)/$(PROGRAM)
	install -o root -g i4psion -m 04110 $(PROGRAM) $(BINDIR)

zoo:
	zoo ah p3nfsd.zoo Makefile tags *.c *.h *.opl CHANGES INSTALL PORTING PROBLEMS README TODO

dist tar: opl
	rm -rf dist; mkdir dist
	dir=p3nfs`sed < version.h -n -e '/#define DIRSUFFIX/s/.* //p'`; \
	mkdir dist/$$dir; \
	ln CHANGES INSTALL PORTING PROBLEMS TODO \
	  Makefile *.opl *.c *.h dist/$$dir; \
	cd dist; tar chf - $$dir | gzip > ../$$dir.tar.gz
	rm -rf dist

tags:
	ctags -tvs *.c *.h

clean celan:
	rm -f $(PROGRAM) $(OBJS) *pure_* .pure

../include/cnv.h: ../nfsc/src/cnv.h
	sed -e 's///' < ../nfsc/src/cnv.h > $@

mp_main.o: mp_main.c ../include/config.h ../include/mp.h ../include/nfs_prot.h ../include/version.h
mp_mount.o: mp_mount.c ../include/config.h ../include/mp.h ../include/nfs_prot.h
nfs_prot_svc.o: nfs_prot_svc.c ../include/mp.h ../include/nfs_prot.h
nfs_prot_xdr.o: nfs_prot_xdr.c ../include/nfs_prot.h
mp_pfs_ops.o: mp_pfs_ops.c ../include/mp.h ../include/nfs_prot.h
mp_serial.o: mp_serial.c ../include/config.h ../include/mp.h ../include/nfs_prot.h
mp_inode.o: mp_inode.c ../include/cnv.h ../include/mp.h ../include/nfs_prot.h
mp_xmit.o: mp_xmit.c ../include/mp.h ../include/nfs_prot.h
crc.o: crc.c
pty.o: pty.c ../include/config.h ../include/mp.h ../include/nfs_prot.h
