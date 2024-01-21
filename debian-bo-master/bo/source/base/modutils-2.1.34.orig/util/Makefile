include ../Makeconfig

#----------------------------------------------------------------------

all: libutil.a

OBJS = sys_cm.o sys_dm.o sys_gks.o sys_nim.o sys_oim.o sys_qm.o \
	xmalloc.o xrealloc.o xstrdup.o

libutil.a: $(OBJS)
	ar rv $@ $?
	$(RANLIB) $@

realclean clean:
	rm -f *.o *.a .depend

install-bin: all

dep depend:
	gcc -M $(CFLAGS) $(DEFS) *.c > .depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif
