#
# Makefile for the iBCS emulator files
#
# $Id: Makefile,v 1.60 1996/11/11 21:20:43 jaggy Exp $
# $Source: /usr/CVS/ibcs/iBCSemul/Makefile,v $
#

######## You should not need to change anything in this file ########

# Build the compile flags etc. based on the selected options in ../CONFIG.
# Edit ../CONFIG if you don't want the defaults!

include ../CONFIG

MODULEDIR = /lib/modules/`uname -r`/misc

EMUOPTS	=

x86_OBJS = emulate.o sysi86.o wysev386.o xnx.o

SPARC_OBJS = solaris.o

OBJS	= $(x86_OBJS) map.o coff.o hrtsys.o ioctl.o ipc.o mmap.o open.o \
	secureware.o socket.o poll.o ptrace.o signal.o socksys.o stat.o \
	stream.o sysconf.o sysfs.o sysinfo.o sysisc.o syslocal.o \
	timod.o ulimit.o utsname.o vtkd.o xstat.o \
	binfmt_lib.o bsdioctl.o


ifeq ($(ARCH),sparc)
OBJS := $(OBJS) $(SPARC_OBJS)
endif

ifeq ($(HAVE_QUOTA),yes)
EMUOPTS := $(EMUOPTS) -DHAVE_QUOTA
endif

ifeq ($(EMU_SCO),yes)
EMUOPTS := $(EMUOPTS) -DEMU_SCO
endif
ifeq ($(EMU_ISC),yes)
EMUOPTS := $(EMUOPTS) -DEMU_ISC
endif
ifeq ($(EMU_SVR4),yes)
EMUOPTS := $(EMUOPTS) -DEMU_SVR4
OBJS	:= $(OBJS) svr4.o
endif
ifeq ($(EMU_BSD),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BSD
OBJS	:= $(OBJS) bsd.o bsdsignal.o bsdsocket.o bsdstat.o
endif
ifeq ($(EMU_WYSE),yes)
EMUOPTS := $(EMUOPTS) -DEMU_WYSE
endif
ifeq ($(EMU_WYSEMP),yes)
EMUOPTS := $(EMUOPTS) -DEMU_WYSEMP
endif
ifeq ($(EMU_X286),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_X286
endif

ifeq ($(EMU_SPX),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_SPX
endif
ifeq ($(EMU_XTI),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_XTI
endif
ifeq ($(EMU_TLI_OPTMGMT),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_TLI_OPTMGMT
endif
ifeq ($(EMU_XTI_OPTMGMT),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_XTI_OPTMGMT
endif

ifeq ($(EMU_BINFMT_ELF),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BINFMT_ELF
OBJS	:= $(OBJS) binfmt_elf.o
endif
ifeq ($(EMU_BINFMT_AOUT),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BINFMT_AOUT
OBJS	:= $(OBJS) binfmt_aout.o
endif
ifeq ($(EMU_BINFMT_COFF),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BINFMT_COFF
OBJS	:= $(OBJS) binfmt_coff.o
endif
ifeq ($(EMU_BINFMT_XOUT),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BINFMT_XOUT
OBJS	:=$(OBJS) binfmt_xout.o
endif
ifeq ($(EMU_BINFMT_OLDSCRIPT),yes)
EMUOPTS	:= $(EMUOPTS) -DEMU_BINFMT_OLDSCRIPT
OBJS	:=$(OBJS) binfmt_script.o
endif

ifeq ($(IBCS_TRACE),yes)
EMUOPTS	:= $(EMUOPTS) -DIBCS_TRACE
endif
ifeq ($(VERBOSE_ERRORS),yes)
EMUOPTS	:= $(EMUOPTS) -DVERBOSE_ERRORS
endif
ifeq ($(COFF_TRACE),yes)
EMUOPTS	:= $(EMUOPTS) -DCOFF_TRACE
endif
ifeq ($(ELF_TRACE),yes)
EMUOPTS	:= $(EMUOPTS) -DELF_TRACE
endif
ifeq ($(XOUT_DEBUG),yes)
EMUOPTS	:= $(EMUOPTS) -DXOUT_DEBUG
endif
ifeq ($(XOUT_TRACE),yes)
EMUOPTS := $(EMUOPTS) -DXOUT_TRACE
endif

ifneq (${ELFMODULE},)
LDMODFLAGS=-m elf_i386
endif

ifeq ($(USE_VERSIONS),yes)
MODOPTS := -DMODVERSIONS -include /usr/include/linux/modversions.h
endif

ifeq ($(SMP),yes)
SMPOPTS := -D__SMP__=1
endif

CFLAGS = -D__KERNEL__=1 -DMODULE -D__NO_VERSION__ \
	-I../include -Wall -Wstrict-prototypes \
	-O3 -fomit-frame-pointer $(ARCH_FLAGS) \
	-DSOCKSYS_MAJOR=$(SOCKSYS_MAJOR) $(EMUOPTS) $(MODOPTS) $(SMPOPTS)


.S.s:
	$(CPP) -traditional $< -o $*.s
.c.s:
	$(CC) $(CFLAGS) -S $<
.s.o:
	$(AS) -c -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -c $<


all:	iBCS

install: all
	-mkdir -p "$(MODULEDIR)"
	cp iBCS "$(MODULEDIR)"/iBCS


iBCS:	$(OBJS)
	$(LD) -r $(LDMODFLAGS) -o iBCS $(OBJS) $(ARCH_LIBS)


clean:
	rm -f core *.o *.a *.s iBCS .depend

dep:
	$(CPP) -M -I../include $(CFLAGS) *.c > .depend

dummy:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
