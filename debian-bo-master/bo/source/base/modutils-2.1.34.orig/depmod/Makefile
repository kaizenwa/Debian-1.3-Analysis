include ../Makeconfig

DEFS += -DELF_MACHINE_H='"elf_$(ARCH).h"'

######################################################################

OBJS = misc.o conf_file.o

all: modprobe depmod

modprobe: modprobe.o $(OBJS) ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

depmod: depmod.o $(OBJS) ../obj/libobj.a ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

clean:
	rm -f *~ core *.bak *.o *.obt *.old
	rm -f depmod modprobe

realclean distclean: clean
	rm -f .depend

install install-bin: all
	$(INSTALL) -c modprobe $(SBINDIR)
	$(INSTALL) -c depmod $(SBINDIR)

dep depend:
	gcc -M $(CFLAGS) $(DEFS) *.c > .depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif
