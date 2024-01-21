#
# Makefile for m68k specific functions.
#

TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR)/include -I..
BASE_CFLAGS:=$(BASE_CFLAGS) -fno-omit-frame-pointer

DIRS:=

SRCS= __longjmp.c __setjmp.c # sigsetjmp.c

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
