#
# Makefile for the linux smb-filesystem utilities.
#

UTILS = smbmount smbumount

COPT = -O2
CFLAGS = $(COPT) $(INCLUDES) $(KERNELD) -DVERSION=\"$(VERSION)\" -Wall
CC = gcc

all: $(UTILS)

$(UTILS): %: %.o
	$(CC) -o $@ $(addsuffix .o,$@)

install: all
	for i in $(UTILS); \
	do install $$i -m 4755 $(BINDIR); done

dep:
	$(CPP) -M $(INCLUDES) *.c > .depend

clean:
	rm -f *.o *~ $(UTILS)

mrproper: clean
	rm -f $(UTILS) .depend $(DISTFILE)

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
