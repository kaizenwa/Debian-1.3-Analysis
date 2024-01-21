GPP=g++
GCC=gcc
all: mangle utstgen utstspc

# builtin test in mangle.c
mangle: /usr/src/linux/fs/umsdos/mangle.c printk.o
	cc -DTEST -D__KERNEL__ -o mangle /linux/fs/umsdos/mangle.c printk.o

# Generate prototype
p:
	proto -co -fumsdos_test.p -c+ -fumsdos_test.pm *.c gen/*.c spc/*.c \
		-bumsdos_test.h+umsdos_test.pm=umsdos_test.h


LIB		= util.o register.o

# General test cases
TESTGEN	= utstgen.o\
		  gen/dev.o gen/dir.o gen/file.o gen/hlink.o gen/perm.o\
		  gen/pipe.o gen/rename.o gen/time.o gen/syml.o\
		  gen/mmap.o gen/multi.o gen/name.o gen/sticky.o #mount.o
utstgen:  $(TESTGEN) $(LIB)
	$(GCC) -g -o utstgen $(TESTGEN) $(LIB)

# UMSDOS specific test cases
TESTSPC	= utstspc.o utilspc.o\
		  spc/hlink.o spc/read.o
utstspc:  $(TESTSPC) $(LIB)
	$(GCC) -g -o utstspc $(TESTSPC) $(LIB)

.c.o:
	$(GPP) -I. -D__KERNEL__ -DSTATIC= -DPUBLIC= -DPRIVATE= -DPROTECTED=  -O2 -g -c $< -o $@


# target to help testing

# format the floppy in a ramdisk and mount
# defined f: in /etc/mtools
# F /dev/ram 12 80 2 18		# ramdisk

mount0:
	mformat f:
	sync
	mount -t umsdos /dev/ram /mnt
	/sbin/umssync /mnt		# This make sure umsdos semantic is installed

mount:
	mount -t umsdos /dev/ram /mnt

cleanemd:
	-umount /mnt
	-mdel f:--linux-.---
	-mdel f:dir/--linux-.---
	mount -t umsdos /dev/ram /mnt

# umount and mount again. Useful if you want to find out if inode
# are properly released
mum:
	umount /mnt
	mdir f:
	mount -t umsdos /dev/ram /mnt

# Apply the general test to a subdir of tmp
# I use the ext2 filesystem as a reference. Mostly to test utstgen
# in the first place.
TST_EXT=/tmp/umsdos.tst
tst.ext:
	mkdir $(TST_EXT)
	utstgen -p$(TST_EXT) all
	rmdir $(TST_EXT)
	

CLEAN=rm -f *.o *.old *~ *.bak utstgen utstspc mangle
clean:
	$(CLEAN)
	(cd gen; $(CLEAN) )
	(cd spc; $(CLEAN) )


