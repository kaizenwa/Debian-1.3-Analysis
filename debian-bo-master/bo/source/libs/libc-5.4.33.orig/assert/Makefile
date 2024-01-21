#
# This is Makefile of assert for Linux
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I$(TOPDIR)/sysdeps/$(TARGET_OS)

DIRS:=
SRC1S= assert-perr.c assert.c
SRC2S=

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets
