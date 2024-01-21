#
#	Makefile for wctype functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I.

DIRS:=

SRCS= wcfuncs.c wctype.c iswctype.c wctrans.c towctrans.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
