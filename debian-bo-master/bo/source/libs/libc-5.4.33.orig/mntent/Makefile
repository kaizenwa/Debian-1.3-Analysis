#
# Makefile for mntent functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS:=

SRC1S= mntent.c
SRC2S= setmntent.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets
