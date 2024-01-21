# BINTYPE must be set to generate QMAGIC a.out binaries in an ELF
# environment. Currently there is *no* support for an x286emul in
# any other format.
BINTYPE=	i486-linuxaout

CC=		cc -pipe
#CFLAGS=		-O -g -I../include -D__KERNEL__ -DDEBUG -DDEBUG_LDT
CFLAGS=		-s -O2 -fomit-frame-pointer -m486 -I../include -D__KERNEL__ -DDEBUG
LDFLAGS=	-static


BOOTOBJS=	boot.o
OBJS=		x286emul.o ldt.o syscall.o lcall7.o \
		emu_generic.o emu_memory.o emu_exec.o \

ifneq ($(BINTYPE),)
CC	:= $(CC) -b $(BINTYPE)
LD	:= /usr/$(BINTYPE)/bin/ld -m i386linux
endif

GCCLIB=		`$(CC) --print-libgcc-file-name`


all:		x286emul

install:	all
		cp x286emul /usr/lib/x286emul

clean:
		rm -f x286emul *.o


x286emul:	$(BOOTOBJS) $(OBJS)
		$(LD) $(LDFLAGS) -qmagic -o $@ \
			$(BOOTOBJS) \
			/usr/$(BINTYPE)/lib/crt0.o \
			$(OBJS) \
			-L/usr/$(BINTYPE)/lib \
			-lc $(GCCLIB) -lc $(GCCLIB)
#		$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(BOOTOBJS) $(OBJS)

lcall7.o:	lcall7.c lcall7.h
		$(CC) -O2 -fomit-frame-pointer -c lcall7.c

dep:
		$(CC) -M -I../include $(CFLAGS) *.c > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
