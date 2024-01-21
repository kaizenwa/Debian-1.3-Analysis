#
# Makefile for m68k specific functions.
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I$(TOPDIR)/string -I$(TOPDIR) -I. 

DIRS:= setjmp

SRC1S= hton.c modf.c bzero.c ffs.c memchr.c memcmp.c memset.c \
	strcat.c strchr.c strcmp.c strcspn.c strlen.c strncmp.c \
	strpbrk.c strrchr.c strspn.c 
SRC2S= ntoh.c div.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SCR2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
