PROG = traceroute

BINDEST = /usr/sbin
MANDEST = /usr/man/man8

CFLAGS = -I. -O2 -Wall -Wno-unused

$(PROG): $(PROG).o
	$(CC) -o $@ $@.o $(LFLAGS)

install: $(PROG)
	cp $(PROG) $(BINDEST)
	cp $(PROG).8 $(MANDEST)
	chmod a=x,u+s $(BINDEST)/$(PROG)

clean:
	$(RM) $(PROG) $(PROG).o
