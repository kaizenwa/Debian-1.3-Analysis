#
# Makefile for Linux files in pthreads
#

TOPDIR=../../..

override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB=libpthreads

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I$(TOPDIR)/sysdeps/linux/$(TARGET_ARCH) -I$(TOPDIR)
BASE_CFLAGS:= $(BASE_CFLAGS) -DTARGET_MACHINE=\"$(TARGET_MACHINE)\" \
	-DPTHREAD_KERNEL -D_LIBPTHREAD $(PTHREAD_CFLAGS)
OPT_CFLAGS:= -g
PIC_OPT_CFLAGS:= -fPIC -g

override SHARED_LIB=$(SHARED_DIR)/libpthreads.a


DIRS:= $(TARGET_ARCH)

# libpthreads
SRC1S=	_fxstat.c dirent.c

# We get them from LINUX_SYSCALL_DIR
VPATH=../../linux

SRC2S=__open.S __write.S __read.S creat.S __close.S __fcntl.S \
	__lseek.S __dup2.S __dup.S __pipe.S __fchmod.S __fchown.S \
	__execve.S __ioctl.S ftruncate.S __sigproc.S __wait4.S \
	getpgrp.S __fork.S __syscall_flock.S __getdents.S \
	__socketcall.S

SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)

include $(TOPDIR)/Maketargets
