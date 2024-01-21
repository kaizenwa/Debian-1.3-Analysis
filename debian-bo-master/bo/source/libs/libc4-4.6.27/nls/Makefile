#
# Makefile for message catalog'ing functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

GENCATOBJ=gencat.o genlib.o mcprt.o mcprtlib.o msgcat.o msgcat-libc.o

INC_CFLAGS=-I.
ifeq ($(CHECKER),true)
BASE_CFLAGS:= $(BASE_CFLAGS) -DI_STDARG
else
BASE_CFLAGS:= $(BASE_CFLAGS) -DHAVE_MMAP -DI_STDARG
endif

DIRS:=

SRCS= msgcat.c msgcat-libc.c #mcprt.c mcprtlib.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets

#.c.o:
#	$(CC) $(CFLAGS) -c $<

#clean:
#	rm -f $(OBJ) *~ *.bak gencat libmsg.a

shar:	msgcat.sh

gencat.o:	gencat.c gencat.h
genlib.o:	genlib.c gencat.h msgcat.h

libmsg.a: $(MSGLIBOBJ)
	$(AR) crv $@ $(MSGLIBOBJ)


msgcat.o:	msgcat.h nl_types.h

mcprt.o:	mcprt.h mcprtlib.h
mcprtlib.o:	mcprtlib.h nl_types.h

msgcat.sh:	$(FILES)
	shar -C $(FILES) > $@

gencat:	$(GENCATOBJ:.o=.c)
	$(CC) $(CFLAGS) -o $@ $(GENCATOBJ:.o=.c)
