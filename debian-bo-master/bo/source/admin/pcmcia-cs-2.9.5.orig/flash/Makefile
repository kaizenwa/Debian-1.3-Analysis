#
# flash/Makefile 1.7 1996/11/26 07:24:56 (David Hinds)
#

# Include site dependent options
include ../config.mk

#ifndef CFLAGS
CFLAGS = -O -Wall -Wstrict-prototypes -pipe -I../include
XFLAGS = -O -pipe -I../include
#endif

COFLAGS = -kv

SRCS = ftl_format.c ftl_check.c
TOOLS = ftl_format ftl_check

all:	$(TOOLS)

dep:	$(SRCS)
	$(CPP) -M -I../include $(SRCS) > .depend

clean:
	rm -f core core.* *.o *.s *.a tmp_make *~
	rm -f $(TOOLS)

install: $(TOOLS)
	mkdir -p $(PREFIX)/sbin
	cp -f $(TOOLS) $(PREFIX)/sbin
