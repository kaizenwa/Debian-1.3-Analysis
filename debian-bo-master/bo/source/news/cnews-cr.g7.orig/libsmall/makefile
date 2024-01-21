MORECFLAGS=-I../relay



# =()<@<INCLUDE>@>()=
.include "../include/config.make"



OBJS=active.slow.o
ALL=$(OBJS)

u:	$(ALL)
	$(UPDATE) $(OBJS)

o:	$(OBJS)

cmp:	$(ALL)
	$(LIBCMP) $(OBJS)

clean:
	rm -f *.o
