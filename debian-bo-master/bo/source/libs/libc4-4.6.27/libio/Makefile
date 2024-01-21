#
# Makefile for libio
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I./ldouble -I./ldouble -I./ldouble -I./ldouble

DIRS=stdio ldouble

SRC1S= filedoalloc.c fileops.c floatconv.c \
	genops.c iofclose.c iofdopen.c iofflush.c iofgetpos.c \
	iofgets.c iofopen.c iofprintf.c iofputs.c iofread.c \
	iofscanf.c iofsetpos.c ioftell.c iofwrite.c iogetline.c \
	iogets.c ioignore.c iopadn.c ioperror.c iopopen.c ioprims.c \
	ioprintf.c ioputs.c ioscanf.c ioseekoff.c ioseekpos.c \
	iosetbuffer.c iosetvbuf.c iosprintf.c iosscanf.c \
	iostrerror.c iotempname.c ioungetc.c \
	iovsprintf.c iovsscanf.c \
	stdfiles.c strops.c iogetdelim.c
# iovfprintf.c iovfscanf.c outfloat.c
SRC2S= cleanup.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SCR2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
