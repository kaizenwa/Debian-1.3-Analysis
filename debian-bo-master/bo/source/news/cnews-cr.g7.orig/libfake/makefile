# =()<@<INCLUDE>@>()=
.include "../include/config.make"

OBJS=$(OFAKE)
ALL=$(OBJS)

u:	$(ALL)
	$(UPDATE) $(OBJS)

o:	$(OBJS)

cmp:	$(ALL)
	$(LIBCMP) $(OBJS)

try:
	$(MAKE) `ls *.c | sed 's/\.c$$/.o/'`

clean:
	rm -f *.o
