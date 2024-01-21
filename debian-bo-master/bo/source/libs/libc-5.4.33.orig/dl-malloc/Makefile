#
# Makefile for malloc library functions
#

override DEBUG=false

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I. -I. -I.
VSCFLAGS=-D_default_morecore=__default_morecore \
	-D__MALLOC_0_RETURNS_NULL -D_morecore=__morecore
VSCFLAGS=-DMALLOC_HOOKS # -DDEBUG
#PIC_OPT_CFLAGS=-O -g -DDEBUG
BASE_CFLAGS := $(BASE_CFLAGS) $(VSCFLAGS)

DIRS:=

SRCS = malloc.c 
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)

include $(TOPDIR)/Maketargets
