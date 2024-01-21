#
# Makefile for grp functions
#

TOPDIR=..

ifeq ($(NYS),true)
YPFLAGS=-DYP
endif

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I. -I$(TOPDIR)

DIRS:=
SRCS= getnetgrent.c
SMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
