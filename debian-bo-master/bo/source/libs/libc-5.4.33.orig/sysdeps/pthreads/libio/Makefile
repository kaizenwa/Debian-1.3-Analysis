#
# Makefile for Linux/x86 files in pthreads
#

TOPDIR=../../..

override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB=libpthreads

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I$(TOPDIR)/sysdeps/libio
BASE_CFLAGS:= $(BASE_CFLAGS) -DTARGET_MACHINE=\"$(TARGET_MACHINE)\" \
	-DPTHREAD_KERNEL -D_LIBPTHREAD $(PTHREAD_CFLAGS)
OPT_CFLAGS:= -g
PIC_OPT_CFLAGS:= -fPIC -g

override SHARED_LIB=$(SHARED_DIR)/libpthreads.a

VPATH=../../../libio/stdio

DIRS:=

# libpthreads
SRC1S= stdio.c clearerr.c fgetc.c fputc.c freopen.c fseek.c getc.c \
	getchar.c getw.c putc.c putchar.c putw.c rewind.c setfileno.c \
	vfscanf.c vscanf.c

SRC2S=

SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)

include $(TOPDIR)/Maketargets
