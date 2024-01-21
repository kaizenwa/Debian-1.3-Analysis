BINDIR  = /usr/sbin
LIBDIR  = 
CFLAGS  = -O2 # -DHAS_SHADOW
LFLAGS  =
OBJECTS = poppassd.o
LIBS    = # -lshadow

poppassd: $(OBJECTS)
	cc -o poppassd $(LFLAGS) $(OBJECTS) $(LIBS)

install: poppassd
	install -o root -g bin -m 500 poppassd $(BINDIR)

clean:
	rm -f *.o *~* core Makefile.new Makefile.bak poppassd

poppassd.o: poppassd.c
	cc -c $(CFLAGS) poppassd.c
