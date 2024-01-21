#
# Makefile for the com_err library
#

OBJECTS = com_err.o error_message.o et_name.o init_et.o
CFLAGS = -Wall -O2

all: $(OBJECTS)

dep:
	$(CPP) -M $(INCLUDES) *.c > .depend

clean:
	rm -f *.o

mrproper: clean
	rm -f .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

