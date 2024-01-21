# This is the Makefile of watchdog
#
# If you don't want to log any activity uncomment the following.
# I strongly discourgae this, though.
#
SYSLOG = -DUSE_SYSLOG
#
# What's the name of your softdog device?
#
DEV = -DDEVNAME=\"/dev/watchdog\"
#
# name of the PID file
#
PID = -DPIDFILE=\"/var/run/watchdog.pid\"
#
# kernel timer margin 
#
TM = -DTIMER_MARGIN=60  
#
# maximal 1 min load average
#
MAX = -DMAXLOAD=12
#
# minimal 1 min load average, the lowest value that is accepted
#
MIN = -DMINLOAD=2
#
# Mount defaults:
#
DEFAULT_FSTYPE=\"iso9660\"
#
# you need rpcgen and libc-4.2 or rpclib to compile in the NFS support
#
MNT = -DHAVE_NFS -DFSTYPE_DEFAULT=$(DEFAULT_FSTYPE)
#
# Were do you want to install watchdog?
#
DESTDIR=
SBINDIR=$(DESTDIR)/usr/sbin
MANDIR=${DESTDIR}/usr/man/man8
#
# You shouldn't have to change anything below here
#
CC=gcc
#
CFLAGS=-g -O2 -Wall ${SYSLOG} ${DEV} ${PID} ${TM} ${MNT} ${MAX} ${MIN} -Imount
LDFLAGS=-s
#
MAJOR_VERSION = 2
MINOR_VERSION = 1
#
OBJECTS=watchdog.o quotactl.o umount.o mount/sundries.o \
	mount/lomount.o mount/fstab.o mount/version.o

all: watchdog

watchdog: $(OBJECTS)

w: w.o quotactl.o umount.o mount/sundries.o mount/lomount.o mount/fstab.o mount/version.o


watchdog.o: version.h watchdog.c

w.o: version.h w.c

umount.o: mount/umount.c
	$(CC) -c $(CFLAGS) -Dmain=umountmain mount/umount.c -o umount.o

version.h: Makefile
	@echo "/* actual version */" > version.h
	@echo "/* DO NOT EDIT! */" >> version.h
	@echo "#define MAJOR_VERSION ${MAJOR_VERSION}" >> version.h
	@echo "#define MINOR_VERSION ${MINOR_VERSION}" >> version.h

install: all
	install -d $(SBINDIR)
	install -g root -o root -m 700 -s watchdog $(SBINDIR)
	install -d $(MANDIR)
	install -g root -o root -m 644 watchdog.8 $(MANDIR)

clean:
	/bin/rm watchdog version.h *.o mount/*.o
