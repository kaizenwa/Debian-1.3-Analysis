include ../Makeconfig

DEFS += -DELF_MACHINE_H='"elf_$(ARCH).h"'

#----------------------------------------------------------------------

all: libobj.a

LIBOBJ_OBJS = obj_common.o obj_load.o obj_reloc.o obj_$(ARCH).o

libobj.a: $(LIBOBJ_OBJS)
	ar rv $@ $?
	$(RANLIB) $@

realclean clean:
	rm -f *.o *.a .depend

install-bin: all

dep depend:
	gcc -M $(CFLAGS) $(DEFS) $(LIBOBJ_OBJS:.o=.c) > .depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif
