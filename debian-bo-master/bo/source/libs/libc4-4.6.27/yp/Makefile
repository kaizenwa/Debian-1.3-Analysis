#
# This is Makefile for YP functions.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I.

DIRS:=

SRCS = yp_xdr.c yppasswd_xdr.c yplib.c ypupdate_xdr.c yp_update.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets  
