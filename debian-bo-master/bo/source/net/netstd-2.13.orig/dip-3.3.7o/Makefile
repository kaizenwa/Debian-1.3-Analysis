#
# dip		Dialup IP connection support program.
#		Makefile for LINUX.
#
# Version:	@(#)Makefile.LINUX	1.30	08/18/94
#
# Author:	Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
#		Copyright 1988-1993 MicroWalt Corporation
#	
# Modified:     Uri Blumenthal, <uri@watson.ibm.com>
#               Copyright 1994 IBM T. J. Watson Research Center
#
#		This program is free software; you can redistribute it
#		and/or  modify it under  the terms of  the GNU General
#		Public  License as  published  by  the  Free  Software
#		Foundation;  either  version 2 of the License, or  (at
#		your option) any later version.
#

# If you want to include S/Key authentication, un-comment the next
# 2 lines. You will need libskey.a from the skey-linux-1.1b package
# (available on sunsite.unc.edu in
# /pub/Linux/system/Network/sunacm/Programs/User/skey)
# If libskey.a is not in /usr/lib then add the path to it in the 
# SKEYLIB define below.
# Some of S/Key sources is here in skey directory, probably enough
# to build libskey.a, but I haven't tested it...
#
# If you're retarded and insist on binary locks - you may try
# -DHAVE_V2_LOCKFILES=1 flag. I haven't debugged it and will
# not accept any complains if this doesn't work as you
# would like (:-). I don't use it myself, won't test
# it and won't care.
#
# If you have all your locks in /var/lock, as FSSTND recommends (and
# that's exactly what I don't follow :-) - you'd need to define
# -DFSSTND and add this define to CFLAGS or such.

# Enable either one only, or none of the following.
# If you use SecureID card, enable this:
#SECUREID=-DSECUREID
#SECUREIDLIB=
# If you have SNK-004 card, enable this:
#SNK=-DSNK
#SNKLIB=

SKEYDEF	= -DSKEY
#SKEYLIB	= -L. -L./skey -L/usr/local/lib -lskey
SKEYLIB	= -lskey

CC	= gcc

# If you wish, add -g flag to the next line
CFLAGS	= -DLINUX $(SKEYDEF) $(SECUREID) -pipe -O6 -Wall -m486

#LD	= ld
LD	= $(CC)

LDFLAGS	= -Wl,-qmagic
#LDFLAGS= -Wl,-verbose -Wl,-qmagic
#LDFLAGS = -v -s

# THE FOLLOWING 6 LINES SHOULDN'T BE USED, UNLESS YOU REDEFINE LD!
# For a.out
#SYSBOBJS = /usr/lib/crt0.o
#SYSEOBJS =
# For ELF
#SYSBOBJS = /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtbegin.o
#SYSEOBJS = /usr/lib/crtend.o /usr/lib/crtn.o

#LIBS	= `$(CC) --print-libgcc-file-name` -lc

# Object modules.
OBJS	= main.o config.o daemon.o tty.o attach.o term.o \
	  modem.o command.o login.o 

PROTOS	= slip.o ppp.o termp.o


.c.o:		dip.h $<
		$(CC) $(CFLAGS) -c $<


all:		dip
	-@echo "DIP is built successfully."

install:	all
	-@cp /usr/sbin/dip ./dip-
	install -c -o root -g uucp -m 04755 -s dip /usr/sbin
	ln -sf /usr/sbin/dip /usr/sbin/diplogin
	install -c -m 0444 dip.8 /usr/man/man8
	-@echo "DIP is installed. Now please install/configure /etc/diphosts."

dip:		protocols.a $(OBJS)
		$(LD) $(LDFLAGS) -o dip $(SYSBOBJS) $(OBJS) protocols.a \
		$(SKEYLIB) $(SECUREIDLIB) $(SNKLIB) $(LIBS) $(SYSEOBJS)

protocols.a:	$(PROTOS)
		ar rcs protocols.a $(PROTOS)

clean:
		rm -f core *.o *.a

clobber:	clean
		rm -f dip

depend:
	gccmakedep $(DEPFLAGS) -- $(CFLAGS) -- *.c

dummy:

# End of Makefile.
# DO NOT DELETE
attach.o: attach.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h /usr/include/net/if.h \
 /usr/include/linux/if.h /usr/include/linux/route.h \
 /usr/include/linux/if_ether.h
command.o: command.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h /usr/include/linux/ip.h \
 /usr/include/linux/udp.h
config.o: config.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
daemon.o: daemon.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
login.o: login.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
main.o: main.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
modem.o: modem.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
ppp.o: ppp.c dip.h /usr/include/sys/types.h /usr/include/linux/types.h \
 /usr/include/asm/types.h /usr/include/sys/bitypes.h \
 /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h \
 /usr/include/libio.h /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
slip.o: slip.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
term.o: term.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
termp.o: termp.c dip.h /usr/include/sys/types.h \
 /usr/include/linux/types.h /usr/include/asm/types.h \
 /usr/include/sys/bitypes.h /usr/include/stdio.h \
 /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/libio.h \
 /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h
tty.o: tty.c dip.h /usr/include/sys/types.h /usr/include/linux/types.h \
 /usr/include/asm/types.h /usr/include/sys/bitypes.h \
 /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h \
 /usr/include/libio.h /usr/include/_G_config.h /usr/include/stdlib.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h \
 /usr/include/errno.h /usr/include/linux/errno.h \
 /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/unistd.h \
 /usr/include/posix_opt.h /usr/include/gnu/types.h \
 /usr/include/confname.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/stdarg.h \
 /usr/include/string.h /usr/include/syslog.h /usr/include/sys/syslog.h \
 /usr/include/fcntl.h /usr/include/linux/fcntl.h \
 /usr/include/asm/fcntl.h /usr/include/termios.h \
 /usr/include/linux/termios.h /usr/include/asm/termios.h \
 /usr/include/signal.h /usr/include/linux/signal.h \
 /usr/include/asm/signal.h /usr/include/dirent.h \
 /usr/include/linux/limits.h /usr/include/linux/dirent.h \
 /usr/include/posix1_lim.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/limits.h \
 /usr/lib/gcc-lib/i486-linux/2.7.2/include/syslimits.h \
 /usr/include/limits.h /usr/include/posix2_lim.h /usr/include/ctype.h \
 /usr/include/endian.h /usr/include/bytesex.h /usr/include/pwd.h \
 /usr/include/sys/stat.h /usr/include/linux/stat.h \
 /usr/include/sys/socket.h /usr/include/linux/socket.h \
 /usr/include/asm/socket.h /usr/include/linux/sockios.h \
 /usr/include/linux/uio.h /usr/include/sys/ioctl.h \
 /usr/include/linux/ioctl.h /usr/include/asm/ioctl.h \
 /usr/include/sys/socketio.h /usr/include/sys/time.h \
 /usr/include/linux/time.h /usr/include/time.h \
 /usr/include/arpa/inet.h /usr/include/sys/param.h \
 /usr/include/linux/param.h /usr/include/asm/param.h \
 /usr/include/netinet/in.h /usr/include/linux/in.h \
 /usr/include/asm/byteorder.h /usr/include/arpa/nameser.h \
 /usr/include/netdb.h /usr/include/paths.h /usr/include/resolv.h \
 /usr/include/net/if_arp.h /usr/include/linux/if_arp.h \
 /usr/include/linux/autoconf.h pathnames.h /usr/include/linux/fs.h \
 /usr/include/linux/linkage.h /usr/include/linux/wait.h \
 /usr/include/linux/vfs.h /usr/include/asm/statfs.h \
 /usr/include/linux/net.h /usr/include/linux/kdev_t.h \
 /usr/include/linux/tty.h /usr/include/linux/serial.h
