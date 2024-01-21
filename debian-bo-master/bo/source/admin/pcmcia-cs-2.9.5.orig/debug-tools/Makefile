#
# debug-tools/Makefile 1.19 1996/11/26 07:32:27 (David Hinds)
#

# Include site dependent options
include ../config.mk

ifndef CFLAGS
CFLAGS = -O -Wall -Wstrict-prototypes -pipe
endif
CPPFLAGS = -I../include -I../modules

COFLAGS = -kv

SRCS = dump_tuples.c dump_i365.c dump_tcic.c dump_cisreg.c

all:	dump_tuples dump_i365 dump_tcic dump_cisreg

dep:	$(SRCS)
	$(CPP) -M $(CPPFLAGS) $(SRCS) > .depend

clean:
	rm -f core core.* *.o *.a tmp_make *~
	for i in *.c;do rm -f `basename $$i .c`.s;done
	rm -f dump_tuples dump_i365 dump_tcic dump_cisreg

install:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
