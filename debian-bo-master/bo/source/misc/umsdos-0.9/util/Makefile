GPP=g++
GCC=gcc
all: umssync

install: all
	install -m 755 -o bin -g bin -s umssync /sbin
	ln -sf /sbin/umssync /sbin/udosctl
	ln -sf /sbin/umssync /sbin/umssetup
	install -m 755 -o bin -g bin umssync.8 /usr/man/man8

# Installation in the L.A.D.R directories
SBIN_LADR=/n/pc_jac/diska/umsdos/base/admin/sbin
install_kit: all
	install --strip -m 755 -o bin -g bin umssync $(SBIN_LADR)
	( cd $(SBIN_LADR); \
		ln -sf umssync udosctl; \
		ln -sf umssync umssetup; \
	)

allprog: udump udosctl
# Linux Utilities
OBJS = main.o umssync.o umssetup.o udosctl.o umsdosio.o mangle.o\
		numconst.o printk.o
umssync: $(OBJS)
	$(GCC) -o umssync $(OBJS)

# Not really useful utilities
# Dump of a --linux-.---
udump: udump.c
	cc -o udump udump.c

# Generate prototype
p:
	proto -co -fumsdos_progs.p -c+ -fumsdos_progs.pm *.c \
		-bumsdos_progs.h+umsdos_progs.pm=umsdos_progs.h

.c.o:
	@echo $<
	@$(GPP) -D__KERNEL__ -DPUBLIC= -DPRIVATE= -DPROTECTED= -O2 -g -c $< -o $@

# Using directly one module from the kernel!
mangle.o: /usr/src/linux/fs/umsdos/mangle.c
	@echo $<
	@$(GCC) -DPUBLIC= -DPRIVATE= -DPROTECTED= -O2 -g -c $< -o $@

clean:
	rm -f *.o *.old *~ *.bak umssync udump udosctl umssetup

