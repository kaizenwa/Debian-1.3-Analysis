#
# This is Makefile of time lib for Linux.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

TIME_FLAGS=-DTZDIR='"/usr/lib/zoneinfo:/usr/share/zoneinfo"' -DALTZONE

INC_CFLAGS=-I.
BASE_CFLAGS := $(BASE_CFLAGS) $(TIME_FLAGS)

DIRS:=

SRC1S= bsdtime.c difftime.c strftime.c strptime.c
SRC2S= getitmr.c gettod.c setitmr.c settod.c adjtime.c timelocal.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets  
