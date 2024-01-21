#
# Makefile for Linux/68k files in pthreads
#

TOPDIR=../../../..

override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB=libpthreads

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I$(TOPDIR)/sysdeps/linux/$(TARGET_ARCH) -I$(TOPDIR)
BASE_CFLAGS:= $(BASE_CFLAGS) -DTARGET_MACHINE=\"$(TARGET_MACHINE)\" \
	-DPTHREAD_KERNEL $(PTHREAD_CFLAGS)
OPT_CFLAGS:= -g
PIC_OPT_CFLAGS:= -fPIC -g

override SHARED_LIB=$(SHARED_DIR)/libpthreads.a


DIRS:=

# libpthreads
SRC1S=	__select.c sigsuspend.c __sigact.c libc_exit.c # syscall.c

# We get them from LINUX_SYSCALL_DIR
VPATH=../../../linux/m68k

SRC2S=

SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)

include $(TOPDIR)/Maketargets
