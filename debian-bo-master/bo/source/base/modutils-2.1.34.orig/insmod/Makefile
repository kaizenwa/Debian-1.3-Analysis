include ../Makeconfig

DEFS += -DELF_MACHINE_H='"elf_$(ARCH).h"'

#----------------------------------------------------------------------

all: insmod rmmod lsmod ksyms

insmod: insmod.o logger.o ../obj/libobj.a ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

rmmod: rmmod.o logger.o ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

lsmod: lsmod.o logger.o ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

ksyms: ksyms.o logger.o ../util/libutil.a
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^

realclean clean:
	rm -f *.o .depend insmod rmmod lsmod ksyms

install install-bin: all
	$(INSTALL) insmod $(SBINDIR)
	$(INSTALL) -c rmmod $(SBINDIR)
	$(INSTALL) -c lsmod $(SBINDIR)
	$(INSTALL) -c ksyms $(SBINDIR)

dep depend:
	gcc -M $(CFLAGS) $(DEFS) *.c > .depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif
