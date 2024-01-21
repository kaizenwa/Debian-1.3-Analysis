#
# Makefile for libio/ldouble
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

NEWSTDIOFLAGS=
INC_CFLAGS=

SRC1S=	iovfscanf.c iovfprintf.c outfloat.c ioldouble.c

SRC2S= 
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SCR2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
