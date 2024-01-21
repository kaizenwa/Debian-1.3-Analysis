#
# This is Makefile of the math lib for Linux
#

MATH=true

override STATIC=false
override SHARED=false
override DEBUG=false
override PROFILE=false

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS:=

SRC1S=
SRC2S= __old_libm.c
SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRCS:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES:=$(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
