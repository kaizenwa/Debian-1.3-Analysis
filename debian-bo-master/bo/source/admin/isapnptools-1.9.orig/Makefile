#
# If you got the binary distribution and want to install the
# precompiled binaries, use
# make install.bin install.man
#
# Define this if you want pnpdump to dump the device configuration
# registers (not normally useful as it is before configuration)
#
#PNPDUMPFLAGS = -DDUMPREGS
#
# Define REALTIME if you want to run with realtime scheduling, this
# cuts down the isolation and configuration time for a single card
# from 6s to less than 100ms.
# NOTE though, if it hangs, no other process will get processor time.
# You can use ^C to kill it.
# This can only be used with recent kernels, and I have only tested
# it on Linux.2.0.10. It certainly won't work on 1.2.13.
# nanosleep only went into the kernel at 1.3.67, but whether it
# worked or not, I know not.
#
# Depending on your libraries, you may need to define the NEED*s.
# For libc-5.2.18, you'll need them both.
# For libc-5.3.12, I suspect you may not want NEEDNANOSLEEP.
#
PNPFLAGS = -DREALTIME -DNEEDSETSCHEDULER -DNEEDNANOSLEEP

INSTALLBINDIR = /sbin
INSTALLMANDIR = /usr/man

CFLAGS = -O2 -Wall $(PNPDUMPFLAGS) $(PNPFLAGS)

all:	pnpdump isapnp

install: pnpdump isapnp install.bin install.man

install.bin: 
	install -s -m 0700 pnpdump $(INSTALLBINDIR)
	install -s -m 0700 isapnp $(INSTALLBINDIR)

install.man:
	install -m 0644 isapnp.8 $(INSTALLMANDIR)/man8
	install -m 0644 isapnp.conf.5 $(INSTALLMANDIR)/man5
	install -m 0644 pnpdump.8 $(INSTALLMANDIR)/man8

pnpdump: pnpdump.o

pnpdump.o: pnp.h Makefile

isapnp: isapnp.o

isapnp.c: isapnp.y pnp.h Makefile
	flex -i -t isapnp.y > isapnp.c

clean:
	rm -f *.o isapnp.c

spotless: clean
	rm -f pnpdump isapnp

