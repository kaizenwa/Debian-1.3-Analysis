#
# Makefile for som generic math functions.
#

override MATH=true

override DEBUG=false
override PROFILE=false

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I. 

DIRS:=

SRC1S= __finite.c
SRC2S = infnan.c isinf.c isnan.c rint.c copysign.c \
	finite.c drem.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRCS:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
