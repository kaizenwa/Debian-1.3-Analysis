#
# This is Makefile of des lib for Linux.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

VSCFLAGS=-DALT_ECB

DIRS:=

SRCS = des_crypt.c des_soft.c des_impl.c
HDRS = des_crypt.h des.h

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets  
