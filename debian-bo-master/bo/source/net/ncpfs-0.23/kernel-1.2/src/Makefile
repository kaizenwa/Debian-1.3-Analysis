#
# Makefile for the linux ncp-filesystem routines.
#

CFLAGS = -Wall -Wstrict-prototypes -O2 -DMODULE -fomit-frame-pointer \
	$(INCLUDES) -DNCPFS_VERSION=\"$(VERSION)\"\
#         -DDEBUG_NCP=1 -DDEBUG_NCP_MALLOC
#         -DDEBUG_NCP_MALLOC

CC     = gcc -D__KERNEL__ -I.
AS     = as
ARCH   = i386

.c.s:
	$(CC) $(CFLAGS) -S $<
.c.o:
	$(CC) $(CFLAGS) -c $<
.s.o:
	$(AS) -o $*.o $<

OBJS=	dir.o inode.o file.o sock.o ioctl.o ncplib_kernel.o mmap.o

all: $(INTERM_BINDIR)/ncpfs.o

$(INTERM_BINDIR)/ncpfs.o: $(OBJS)
	$(LD) -r -o $@ $(OBJS)

ncplib_kernel.o: ncplib_kernel.c ncplib_kernel.h
	$(CC) $(CFLAGS) -finline-functions -c $<

dep:
	$(CPP) -M $(INCLUDES) *.c > .depend

clean:
	rm -f *.o *~

realclean: clean
	rm -f ncpmount ncptest .depend $(DISTFILE) *.out

modules: ncpfs.o

SRCPATH=$(shell pwd)
SRCDIR=$(shell basename $(SRCPATH))
DISTFILE=$(SRCDIR).tgz
BACKUPFILE=ncpfs01.tgz
HOME=/home/me

backup: 
	(rm -f $(DISTFILE); cd ..; tar cvf - $(SRCDIR) | gzip -1 \
          > $(HOME)/tarz/backup/$(BACKUPFILE))
	(cd $(HOME)/tarz/backup; ls -l $(BACKUPFILE); mcopy $(BACKUPFILE) a:)

dist: realclean
	rm -fr mnt
	(cd ..; \
         tar cvf - $(SRCDIR) | \
            gzip -9 > $(DISTFILE); \
         mv $(DISTFILE) $(SRCDIR))

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
