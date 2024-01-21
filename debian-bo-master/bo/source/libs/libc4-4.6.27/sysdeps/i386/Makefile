#
# Makefile for i386 specific functions.
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR)/string -I$(TOPDIR) -I. 

DIRS:= setjmp

SRC1S= hton.c modf.c bzero.c ffs.c memchr.c memcmp.c memset.c \
	strcat.c strchr.c strcmp.c strcspn.c strlen.c strncmp.c \
	strpbrk.c strrchr.c strspn.c __stpncpy.c stpcpy.c strstr.c
SRC2S= ntoh.c div.c
SRC3S= frexp.S

SRCS= $(SRC1S) $(SRC2S) $(SRC3S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s) $(SRC3S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC3S:.S=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
